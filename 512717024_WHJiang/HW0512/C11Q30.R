##############################################################################
## Klein Model I – Question 11.30  (a)–(f)
##############################################################################
library(tidyverse); library(AER); library(lmtest); library(sandwich)

klein <- read_csv("klein.csv") |>
         rename(I = i, P = p, PLAG = plag, KLAG = klag,
                G = g, W2 = w2, TX = tx, TIME = time, ELAG = elag)

# (a) OLS --------------------------------------------------------------------
ols.inv <- lm(I ~ P + PLAG + KLAG, data = klein)
coeftest(ols.inv)                     # robust 可用 vcovHC(ols.inv,"HC1")

# (b) Reduced form for P -----------------------------------------------------
rf.P <- lm(P ~ G + W2 + TX + TIME + PLAG + KLAG + ELAG, data = klein)
# F-test (G,W2,TX,TIME,ELAG = 0)
linearHypothesis(rf.P, c("G=0","W2=0","TX=0","TIME=0","ELAG=0"))
klein <- klein |>
         mutate(vhat = resid(rf.P),
                Phat = fitted(rf.P))

# (c) Hausman test -----------------------------------------------------------
haus.mod <- lm(I ~ P + PLAG + KLAG + vhat, data = klein)
coeftest(haus.mod)["vhat",]   # t-test on δ

# (d) 2SLS -------------------------------------------------------------------
iv.inv <- ivreg(I ~ P + PLAG + KLAG | G + W2 + TX + TIME + PLAG + KLAG + ELAG,
                data = klein)
summary(iv.inv, diagnostics = TRUE)    # 第一階段 F 與 Wu–Hausman

# (e) Second-stage OLS on Phat ----------------------------------------------
sec <- lm(I ~ Phat + PLAG + KLAG, data = klein)
coeftest(sec)

# (f) Sargan over-ID test ----------------------------------------------------
klein$e2hat <- resid(sec)
aux <- lm(e2hat ~ G + W2 + TX + TIME + ELAG, data = klein) # 5 surplus IVs
T  <- nrow(klein); R2 <- summary(aux)$r.squared
TR2 <- T * R2
cat("Sargan TR^2 =", TR2, "\n")
