library(POE5Rdata)
POE5Rdata::truffles
install.packages("AER")
library(AER)

#11.28(b)
demand_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles)
summary(demand_2sls)

supply_2sls <- ivreg(p ~ q + pf | ps + di + pf, data = truffles)
summary(supply_2sls)

#11.28(c)
mean_P <- mean(truffles$p)
mean_Q <- mean(truffles$q)
coef_demand <- coef(demand_2sls)
beta_q <- coef_demand["q"]
elasticity <- beta_q * (mean_Q / mean_P)
cat("價格彈性為：", round(elasticity, 4), "\n")

#11.28(d)
q_range <- seq(min(truffles$q), max(truffles$q), length.out = 100)
P_demand <- -11.428 - 2.671 * q_range + 3.461 * 22 + 13.39 * 3.5
P_supply <- -58.7982 + 2.9367 * q_range + 2.9585 * 23
plot(q_range, P_demand, type = "l", col = "blue", ylim = range(c(P_demand, P_supply)), ylab = "Price", xlab = "Quantity", main = "Demand and Supply")
lines(q_range, P_supply, col = "red")
legend("topright", legend = c("Demand", "Supply"), col = c("blue", "red"), lty = 1)

#11.28(e)
price_gap <- function(Q) {
  Pd <- -11.428 - 2.671 * Q + 3.461 * 22 + 13.39 * 3.5
  Ps <- -58.7982 + 2.9367 * Q + 2.9585 * 23
  return(Pd - Ps)
}
eq_Q <- uniroot(price_gap, c(min(q_range), max(q_range)))$root
eq_P <- -11.428 - 2.671 * eq_Q + 3.461 * 22 + 13.39 * 3.5

cat("均衡數量 Q =", round(eq_Q, 2), "\n均衡價格 P =", round(eq_P, 2), "\n")

#11.28(f)
demand_ols <- lm(p ~ q+ pf + di, data = truffles)
supply_ols <- lm(p ~ q+ pf , data = truffles)
summary(demand_ols)
summary(supply_ols)

#11.30(a)
data("klein")
summary(klein)
library(car)   
library(lmtest)  
library(sandwich)
library(ggplot2)
install.packages(c("tidyverse","AER","systemfit","broom"))
library(systemfit)  
library(tidyverse)
library(broom)
library(dplyr)
install.packages("plm") 
library(plm)  

ols_inv <- lm(i ~ p + plag + klag, data = klein)
cat("\n(a)  OLS estimates of 11.18:\n")
summary(ols_inv)
print(tidy(ols_inv), n = Inf) 

#11.30(b)
rf_P <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = klein)
summary(rf_P)


joint_test <- linearHypothesis(
  rf_P,
  c("g = 0", "w2 = 0", "tx = 0", "time = 0", "elag = 0")
)
cat("\nJoint F-test (g, w2, tx, time, elag):\n")
print(joint_test, digits = 4)

F_stat <- joint_test$F[2]       
df1    <- joint_test$Df[2]      
df2    <- joint_test$Res.Df[2]  
F_crit <- qf(0.95, df1, df2)   

cat("F statistic =", round(F_stat, 4), "\n")
cat("Critical F(5,13;0.95) =", round(F_crit, 4), "\n")

library(AER) ; data("KleinI")

df <- klein |>
  as.data.frame() |>
  select(year, cn, i, p, plag, klag, e, elag, w2, g, tx, time) |>
  na.omit()                     
nrow(df)  

rf_P  <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = df)
df$phat <- fitted(rf_P)
df$vhat <- resid(rf_P)
df$phat

#11.30(c)
hausman <- lm(i ~ p + plag + klag + vhat, data = df)
cat("\n(c)  Hausman test (t-stat of vhat):\n")
print(coeftest(hausman, vcov. = vcovHC(hausman, type = "HC1"))["vhat", ])
summary(hausman)

#(d)
iv_inv <- ivreg(i ~ p + plag + klag |
                  g + w2 + tx + time + plag + klag + elag,
                data = df)
cat("\n(d) 2SLS estimates of 11.18:\n")
print(tidy(iv_inv), n = Inf)
summary(iv_inv)

compare_slopes <- bind_rows(
  tidy(ols_inv) |> mutate(model = "OLS"),
  tidy(iv_inv)  |> mutate(model = "2SLS")
) |>
  select(model, term, estimate, std.error, statistic, p.value)

cat("\nOLS vs 2SLS coefficients:\n")
print(compare_slopes, n = Inf)

#(e)
stage2 <- lm(i ~ phat + plag + klag, data = df)
cat("\n(e)  Second-stage OLS (I_t on p̂):\n")
summary(stage2)

#(f)
e2hat <- resid(iv_inv)
sargan_aux <- lm(e2hat ~ g + w2 + tx + time + elag + plag + klag, data = df)
R2  <- summary(sargan_aux)$r.squared
TR2 <- nrow(df) * R2
df_sargan <- 4                      # L-B = 5-1 = 4
crit95 <- qchisq(0.95, df_sargan)

cat("\n(f)  Sargan test:\n")
cat("   TR^2  =", round(TR2, 3), "\n")
cat("   χ²_0.95(df=4) =", round(crit95, 3), "\n")
if (TR2 < crit95) {
  cat("   → Fail to reject H0")
} else {
  cat("   → Reject H0 ")
}

#15.17(a)
liquor5 <- liquor5[order(liquor5$hh, liquor5$year), ]

liquor5$liquord <- with(liquor5, ave(liquor, hh, FUN = function(x) c(NA, diff(x))))
liquor5$incomed <- with(liquor5, ave(income, hh, FUN = function(x) c(NA, diff(x))))

df <- na.omit(liquor5[, c("liquord", "incomed")])

model <- lm(liquord ~ incomed - 1, data = df)

cat("\nOLS 回歸結果：\n")
print(summary(model))

conf_int <- confint(model, level = 0.95)
formatted_conf_int <- sprintf("%-10s %10.8f %10.8f", rownames(conf_int), conf_int[, 1], conf_int[, 2])

cat("\n95% 信賴區間：\n")
cat("          2.5 %        97.5 %\n")
cat(formatted_conf_int, sep = "\n")

#15.20(a)
data("star")
summary(star)
ols_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = star)
summary(ols_a)

#15.20(b)
pdata <- pdata.frame(star, index = c("schid","id"))

fe_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pdata,
            model = "within")
summary(fe_b)

#(c)
pool_c <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
              data = pdata,
              model = "pooling")

pFtest(fe_b, pool_c)