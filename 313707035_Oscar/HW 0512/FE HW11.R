library(POE5Rdata)

#11.28

data("truffles")
library(AER)       # contains ivreg()

#(b)Estimate the inverse demand and supply with 2SLS
## 2SLS – inverse demand (p ~ q + ps + di), instrument q with pf
demand.iv <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand.iv)

## 2SLS – inverse supply (p ~ q + pf), instrument q with ps & di
supply.iv <- ivreg(p ~ q + pf        | pf + ps + di, data = truffles)
summary(supply.iv)

#(c)Price-elasticity of demand “at the means”
with(truffles, {
  Pbar <- mean(p);   Qbar <- mean(q)
  alpha2 <- 1 / coef(demand.iv)["q"]
  elas  <- alpha2 * Pbar / Qbar
  round(elas, 3)
})

#(d) Draw the two curves with the prescribed exogenous values
library(ggplot2)

DI0 <- 3.5; PS0 <- 22; PF0 <- 23

invDem <- function(q) coef(demand.iv)["(Intercept)"] +
  coef(demand.iv)["q"]  * q +
  coef(demand.iv)["ps"] * PS0 +
  coef(demand.iv)["di"] * DI0

invSup <- function(q) coef(supply.iv)["(Intercept)"] +
  coef(supply.iv)["q"]  * q +
  coef(supply.iv)["pf"] * PF0

curveData <- data.frame(
  q = seq(0, 35, length = 200)
) |> transform(
  Pd = invDem(q),
  Ps = invSup(q)
)

ggplot(curveData, aes(q)) +
  geom_line(aes(y = Pd), size = 1.1, linetype = "dashed",  ) +
  geom_line(aes(y = Ps), size = 1.1, color = "black") +
  labs(x = "Quantity (q)", y = "Price (p)",
       title = "Inverse Demand (dashed) and Supply (solid)") +
  theme_minimal()

#(e) Solve for the structural equilibrium and compare with the reduced-form prediction
# Intersection of the two straight lines
delta0 <- coef(demand.iv)["(Intercept)"] + coef(demand.iv)["ps"]*PS0 + coef(demand.iv)["di"]*DI0
gamma0 <- coef(supply.iv)["(Intercept)"] + coef(supply.iv)["pf"]*PF0
delta1 <- coef(demand.iv)["q"]
gamma1 <- coef(supply.iv)["q"]

q_star <- (delta0 - gamma0) / (gamma1 - delta1)
p_star <- invDem(q_star)          # or invSup(q_star)

c(p_star = round(p_star, 2), q_star = round(q_star, 2))

#(f) OLS estimates when simultaneity is ignored
demand.ols <- lm(p ~ q + ps + di, data = truffles)
supply.ols <- lm(p ~ q + pf,      data = truffles)

summary(demand.ols)
summary(supply.ols)

#11.30

## ── packages and data ───────────────────────────────────────
library(systemfit)   ; data("KleinI")
library(AER)         ; library(lmtest) ; library(sandwich)

# rename to symbols used here
klein <- transform(KleinI,
                   CN  = consump,           # personal consumption
                   I   = invest,                # private investment
                   TIME= year - 1931,           # trend starting at 0
                   K_1 = capitalLag,            # lagged capital
                   P_1 = corpProfLag,           # lagged profits  (instrument only)
                   G   = govExp,  Wg = govWage, T = taxes,  X_1 = gnpLag)
klein <- na.omit(klein[
  , c("CN","I","TIME","K_1","P_1","G","Wg","T","X_1")])

#(a) OLS estimates of the investment function 
ols.inv <- lm(I ~ CN + K_1, data = klein)
coeftest(ols.inv)

#(b)Reduced-form
rf.cn <- lm(CN ~ TIME + K_1 + G + T + Wg + X_1 + P_1, data = klein)
summary(rf.cn)$r.squared          # ≈ 0.88
# joint significance of the five policy/lag instruments
linearHypothesis(rf.cn, c("G=0","T=0","Wg=0","X_1=0","P_1=0"))

klein$vhat <- resid(rf.cn)        # v̂_t
klein$CNhat<- fitted(rf.cn)       # C N ̂_t

#(c)Hausman test
haus <- lm(I ~ CN + K_1 + vhat, data = klein)
coeftest(haus)["vhat", ]          # δ̂,  t-value

#(d)2SLS estimates of the investment equation
iv.inv <- ivreg(
  I  ~ CN + K_1                     # ← 結構方程右邊的變數 (CN 是內生，K_1 外生)
  | K_1 + TIME + G + T + Wg + X_1 + P_1,   # ← 全部工具變數 (外生＋額外 IVs)
  data = klein)
coeftest(iv.inv, vcov = sandwich)

#(e)Second-stage “plug-in” regression
ols.2nd <- lm(I ~ CNhat + K_1, data = klein)
coeftest(ols.2nd)

#(f)Sargan test of over-identifying restrictions
ehat <- resid(iv.inv)
sarg <- lm(ehat ~ TIME + K_1 + G + T + Wg + X_1 + P_1, data = klein)
TR2  <- nrow(klein) * summary(sarg)$r.squared
df   <- 6
c("TR^2"=TR2, "p-value"=1 - pchisq(TR2, df))
