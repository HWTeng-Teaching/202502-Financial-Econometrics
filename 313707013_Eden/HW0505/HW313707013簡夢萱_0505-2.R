#11.28.b
library(POE5Rdata)
install.packages("AER")
library(AER)
data("truffles")
str(truffles)
install.packages("systemfit")
library(systemfit)
truffle.d <- p ~ q + ps + di     
truffle.s <- p ~ q + pf           
truffle.eqs <- list(truffle.d,truffle.s)     
truffle.ivs <- ~ps + di + pf
truffle.sys <- systemfit(truffle.eqs,
                     method = "2SLS",
                     inst   = truffle.ivs,
                     data   = truffles)
summary(truffle.sys)
#c
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles) 
delta <- coef(demand_2sls)["q"]
mean_P <- mean(truffles$p)
mean_Q <- mean(truffles$q)
elasticity_demand <- (1 / delta) * (mean_P / mean_Q)
cat("Price Elasticity of Demand at the Means:", elasticity_demand, "\n")

#d
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles) 
supply_2sls <- ivreg(p ~ q + pf      | ps + di + pf, data = truffles)
Q_vals <- seq(0, 30, by = 1)
DI_star <- 3.5
PS_star <- 22
PF_star <- 23
coef_demand <- coef(demand_2sls)
a0 <- coef_demand["(Intercept)"]
a1 <- coef_demand["q"]
a2 <- coef_demand["ps"]
a3 <- coef_demand["di"]
coef_supply <- coef(supply_2sls)
b0 <- coef_supply["(Intercept)"]
b1 <- coef_supply["q"]
b2 <- coef_supply["pf"]

P_demand <- a0 + a1 * Q_vals + a2 * PS_star + a3 * DI_star
P_supply <- b0 + b1 * Q_vals + b2 * PF_star
plot(Q_vals, P_demand, type = "l", col = "blue", lwd = 2,
     ylim = range(c(P_demand, P_supply)),
     xlab = "Quantity (Q)", ylab = "Price (P)",
     main = "Supply and Demand for Truffles")

lines(Q_vals, P_supply, col = "red", lwd = 2)
legend("topleft", legend = c("Demand", "Supply"),
       col = c("blue", "red"), lty = 1, lwd = 2)

#e
聯立解方程：
eq_Q <- ((b0 + b2 * PF_star ) - (a0 + a2 * PS_star + a3 * DI_star)) / (a1 - b1)
eq_P <- (a0 + a2 * PS_star + a3 * DI_star) + a1 * eq_Q  # 代回任一條式子都可以
cat("(Q*):", round(eq_Q, 2), "\n")
cat("(P*):", round(eq_P, 2), "\n")

# Reduced-form 模型
# Reduced-form for Q
Reduced_q <- lm(q ~ ps + di + pf, data = truffles)
coef_q <- coef(Reduced_q)
# Reduced-form for P
Reduced_p <- lm(p ~ ps + di + pf, data = truffles)
coef_p <- coef(Reduced_p)
# 計算 predicted Q and P using exogenous values
Reduced_Q <- coef_q["(Intercept)"] + coef_q["ps"] * PS_star + coef_q["di"] * DI_star + coef_q["pf"] * PF_star
Reduced_P <- coef_p["(Intercept)"] + coef_p["ps"] * PS_star + coef_p["di"] * DI_star + coef_p["pf"] * PF_star
cat("Reduced-form Q:", round(Reduced_Q, 2), "\n")
cat("Reduced-form P:", round(Reduced_P, 2), "\n")

#f
OLS_demand <- lm(p ~ q + ps + di, data = truffles)
summary(OLS_demand)
OLS_supply <- lm(p ~ q + pf, data = truffles)
summary(OLS_supply)


#11.30
#a
data("klein")
str(klein)
klein <- na.omit(klein)
inv_ols <- lm(i ~ p + plag + klag, data = klein)
summary(inv_ols)
#b
red_profit_model <- lm(p ~ g + w2 + tx + plag + klag + time + elag, data = klein)
summary(red_profit_model)
restricted_model <- lm(p ~ plag + klag, data = klein)
anova(restricted_model, red_profit_model)
klein$vt_hat <- residuals(red_profit_model)
klein$pt_hat <- fitted(red_profit_model)
klein[, c("p", "pt_hat", "vt_hat")]
#c
hau_model <- lm(i ~ p + plag + klag + vt_hat, data = klein)
summary(hau_model)
#d
library(AER)
iv2sls_model <- ivreg(i ~ p + plag + klag | g + w2 + tx + plag + klag + time + elag, data = klein)
summary(iv2sls_model)
#e
second_stage <- lm(i ~ pt_hat + plag + klag, data = klein)
summary(second_stage)
#f
klein$e2hat <- residuals(second_stage)
sargan_reg_model <- lm(e2hat ~ g + w2 + tx + plag + klag + time + elag, data = klein)
summary(sargan_reg_model)
R2 <- summary(sargan_reg_model)$r.squared
T <- nrow(klein)  # number of observations
TR2 <- T * R2
cat("Sargan test statistic: ", TR2, "\n")
crit <- qchisq(0.95, df = 4)
cat("Critical value (chi^2(4), 95%): ", crit, "\n")
