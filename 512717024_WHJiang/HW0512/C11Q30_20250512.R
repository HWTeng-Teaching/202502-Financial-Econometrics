#  C11Q30 Klein Model I 

### 環境與套件
rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  AER, lmtest, sandwich, car,          # 計量工具
  dplyr, magrittr, broom,            
  modelsummary,                       # 排版輸出
  POE5Rdata                           # Klein 資料
)

### 載入／整理資料
klein <- POE5Rdata::klein %>%
  rename(I = i, P = p, PLAG = plag, KLAG = klag,
         G = g, W2 = w2, TX = tx, TIME = time, ELAG = elag) %>%
  filter(complete.cases(I, P, PLAG, KLAG, G, W2, TX, TIME, ELAG))

### 主要模型
# (1) OLS ------------------------------------------------------------------
ols.inv <- lm(I ~ P + PLAG + KLAG, data = klein)

# (2) 2SLS -----------------------------------------------------------------
iv.inv  <- ivreg(I ~ P + PLAG + KLAG |
                   G + W2 + TX + TIME + PLAG + KLAG + ELAG,
                 data = klein)

# ===== Phat、vhat =====
rf.P <- lm(P ~ G + W2 + TX + TIME + PLAG + KLAG + ELAG, data = klein)
klein <- klein %>%
  mutate(Phat = predict(rf.P, newdata = .),
         vhat = P - Phat)

# (3) Two‑step -------------------------------------------------------------
sec     <- lm(I ~ Phat + PLAG + KLAG, data = klein)

# (4) P RF  (Reduced‑form for P) ------------------------------------------
# 在表格中顯示 exogenous 變數對 P 的影響
p_rf    <- rf.P  # 已於上方估計

# (5) Hausman ------------------------------------------------------------
haus.mod <- lm(I ~ P + PLAG + KLAG + vhat, data = klein)

# (6) Sargan  (過度識別檢定輔助回歸)(使用 2SLS 殘差，含所有工具變數) -----
klein$ehat <- resid(iv.inv)
aux <- lm(ehat ~ PLAG + KLAG + G + W2 + TX + TIME + ELAG, data = klein)

### 統計量整理
abs_tval <- function(m) abs(coef(summary(m))[ , "t value"])
models   <- list("OLS"      = ols.inv,
                 "2SLS"     = iv.inv,
                 "Two steps"= sec,
                 "P RF"     = p_rf,
                 "Hausman"  = haus.mod,
                 "Sargan"   = aux)

stat_override <- lapply(models, abs_tval)
for(i in seq_along(stat_override))
  names(stat_override[[i]]) <- rownames(coef(summary(models[[i]])))

### 係數名稱對應
coef_map <- c("(Intercept)" = "C",
              "P"          = "P",
              "PLAG"       = "PLAG",
              "KLAG"       = "KLAG",
              "W2"         = "W2",
              "G"          = "G",
              "TX"         = "TX",
              "TIME"       = "TIME",
              "ELAG"       = "ELAG",
              "Phat"       = "PHAT",
              "vhat"       = "VHAT")

### 輸出表格
msummary(
  models,
  coef_map          = coef_map,
  statistic_override= stat_override,
  statistic         = "({statistic})",   # 以括號包裹 |t|
  estimate          = "{estimate}{stars}",
  stars             = c("*"=.05,"**"=.01,"***"=.001),
  gof_omit          = "AIC|BIC|Log.Lik|RMSE|SER|Deviance|Adj",
  title             = "Table: Investment Equation & Diagnostics"
)
## 額外檢定輸出
cat("\n--- Additional diagnostics ---\n")
# Weak‑IV F‑stat
f_test <- car::linearHypothesis(rf.P, c("G=0","W2=0","TX=0","TIME=0","ELAG=0"))
print(f_test)
# Hausman coefficient on vhat
cat("\nHausman δ (vhat)  =", coef(summary(haus.mod))["vhat","Estimate"],
    ",  t =", coef(summary(haus.mod))["vhat","t value"],"\n")
# Sargan TR²
TR2  <- nobs(sec) * summary(aux)$r.squared
pval <- 1 - pchisq(TR2, df = 5)
cat("Sargan TR² =", round(TR2,3),",  p-value =", round(pval,4),"\n")