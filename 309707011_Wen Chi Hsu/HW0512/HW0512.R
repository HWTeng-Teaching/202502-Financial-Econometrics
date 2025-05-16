#11.28
install.packages("AER") 
library(AER)
install.packages("ggplot2") 
library(ggplot2)
install.packages(c("tidyverse","AER","systemfit","broom"))
library(AER)         
library(systemfit)  
library(tidyverse)
library(broom)
install.packages("dplyr") 
library(dplyr)
install.packages("plm") 
library(plm)  
install.packages("systemfit")
library(systemfit)
data("truffles")  
summary(truffles)

#b
d <- p ~ q + ps + di     
s <- p ~ q + pf           
inst <- ~ ps + di + pf       

twosls <- systemfit(list(demand = d,
                          supply = s),
                     method = "2SLS",
                     inst   = inst,
                     data   = truffles)
summary(twosls)

#c
d_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles) 
s_2sls <- ivreg(p ~ q + pf      | ps + di + pf, data = truffles)
beta_Q <- coef(d_2sls)["q"]

mean_P <- mean(truffles$p)
mean_Q <- mean(truffles$q)

elasticity <- (1 / beta_Q) * (mean_P / mean_Q)
elasticity

#d
install.packages("tibble") 
library(tibble)
install.packages("tidyverse")
library(tidyverse)

coef_demand <- coef(d_2sls)
a0 <- coef_demand["(Intercept)"]
beta_Q <- coef_demand["q"]
a2 <- coef_demand["ps"]
a3 <- coef_demand["di"]

coef_supply <- coef(s_2sls)
b0 <- coef_supply["(Intercept)"]
beta_sQ <- coef_supply["q"]
b2 <- coef_supply["pf"]

PS_star <- mean(truffles$ps)
DI_star <- mean(truffles$di)
PF_star <- mean(truffles$pf)

Q_vals <- seq(min(truffles$q) * 0.9,
              max(truffles$q) * 1.1,
              length.out = 100)

P_demand <- a0 + beta_Q * Q_vals + a2 * PS_star + a3 * DI_star
P_supply <- b0 + beta_sQ * Q_vals + b2 * PF_star

plot(Q_vals, P_demand, type = "l", col = "blue", lwd = 2,
     ylim = range(c(P_demand, P_supply)),
     xlab = "Quantity (Q)", ylab = "Price (P)",
     main = "Supply and Demand for Truffles")

lines(Q_vals, P_supply, col = "red", lwd = 2)

legend("topleft", legend = c("Demand", "Supply"),
       col = c("blue", "red"), lty = 1, lwd = 2)

#e
rhs_demand <- coef_demand["(Intercept)"] + coef_demand["ps"] * 22 + coef_demand["di"] * 3.5
rhs_supply <- coef_supply["(Intercept)"] + coef_supply["pf"] * 23

eq_Q <- (rhs_supply - rhs_demand) / (a1 - b1)
eq_P <- rhs_demand + a1 * eq_Q  

cat("Equilibrium Quantity (Q*):",eq_Q, "\n")
cat("Equilibrium Price (P*):", eq_P, "\n")

rf_q <- lm(q ~ ps + di + pf, data = truffles)
coef_q <- coef(rf_q)

rf_p <- lm(p ~ ps + di + pf, data = truffles)
coef_p <- coef(rf_p)

Q_rf <- coef_q["(Intercept)"] + coef_q["ps"] * PS_star + coef_q["di"] * DI_star + coef_q["pf"] * PF_star
P_rf <- coef_p["(Intercept)"] + coef_p["ps"] * PS_star + coef_p["di"] * DI_star + coef_p["pf"] * PF_star

cat("Reduced-form predicted Q:",Q_rf, "\n")
cat("Reduced-form predicted P:", P_rf,"\n")

#f
ols_d <- lm(p ~ q + ps + di, data = truffles)
summary(ols_d)
ols_s <- lm(p ~ q + pf, data = truffles)
summary(ols_s)

#11.30
#a
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("klein")
summary(klein)
library(car)   
library(lmtest)  
library(sandwich)

klein <- na.omit(klein)
inv_ols <- lm(i ~ p + plag + klag, data = klein)
summary(inv_ols)

#b
ori_model <- lm(p ~ g + w2 + tx + plag + klag + time + elag, data = klein)
restricted_model <- lm(p ~ plag + klag, data = klein)
anova(restricted_model, ori_model)
cat('Conclusion:\nThe result (F = 1.93, p = 0.1566) indicates that the additional variables are not jointly statistically significant at the 5% level. Thus, conditional on lagged profit and capital stock, these exogenous variables do not significantly improve the explanatory power of the model.')
summary(reduced_p)

klein$uhat <- resid(ori_model)     
klein$Phat <- fitted(ori_model)    
klein[, c("p", "Phat", "uhat")]

#c
hausman_test <- lm(i ~ p + plag + klag + uhat, data = klein)
summary(hausman_test)

#d
library(AER)
iv_2sls <- ivreg(i ~ p + plag + klag | g + w2 + tx + elag + time + plag + klag, data = klein)
summary(iv_2sls)

#e
second_stage <- lm(i ~ Phat + plag + klag, data = klein)
summary(second_stage)

#f
klein$e2_hat <- residuals(second_stage)
sargan_reg <- lm(e2_hat ~ g + w2 + tx + plag + klag + time + elag, data = klein)
summary(sargan_reg)
R2 <- summary(sargan_reg)$r.squared
T1 <- nrow(klein)  

Sargan_stat <- T1 * R2
cat("Sargan test statistic: ", Sargan_stat, "\n")

crit_val <- qchisq(0.95, df = 4)
cat("Critical value (chi^2(4), 95%): ", crit_val, "\n")

#15.17
#a
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("liquor5")
library(dplyr)

liquor5 <- liquor5 %>%
  arrange(hh, year) %>%           
  group_by(hh) %>%               
  mutate(liquord = liquor - lag(liquor),  
         incomed = income - lag(income)) %>%  
  ungroup() %>%
  filter(!is.na(liquord) & !is.na(incomed))  

model <- lm(liquord ~ incomed -1, data = liquor5)

summary(model)

conf_interval <- confint(model, "incomed", level = 0.95)
print(conf_interval)

#15.20
#a
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("star")

model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch
              , data = star)
summary(model_a)

#b
install.packages("plm")
library(plm)
star$sid <- star$id
newdata <- pdata.frame(star, index = c("schid", "sid"))

model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch
              , data = newdata, model = "within")
summary(model_b)

#c
pool <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
              data = newdata, model = "pooling")
pFtest(model_b, pool)