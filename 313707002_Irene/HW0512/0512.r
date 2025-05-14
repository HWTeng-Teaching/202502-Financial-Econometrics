remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("truffles")
library(ggplot2)
summary(truffles)
install.packages(c("tidyverse","AER","systemfit","broom"))
library(AER)         
library(systemfit)  
library(tidyverse)
library(broom)
library(dplyr)
install.packages("plm") 
library(plm)  

#11.28.b
eq_d <- p ~ q + ps + di     
eq_s <- p ~ q + pf           
inst <- ~ ps + di + pf       

sys2sls <- systemfit(list(demand = eq_d,
                          supply = eq_s),
                     method = "2SLS",
                     inst   = inst,
                     data   = truffles)
summary(sys2sls)

#11.28.c
dem_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles) 
sup_2sls <- ivreg(p ~ q + pf      | ps + di + pf, data = truffles)
lambda1 <- coef(dem_2sls)["q"]
Pbar <- mean(truffles$p)
Qbar <- mean(truffles$q)

elas <- (1/lambda1) * (Pbar / Qbar)
elas

#11.28.d
Q_grid <- tibble(Q = seq(min(truffles$q)*0.9, max(truffles$q)*1.1, length.out = 100))

a0 <- coef(dem_2sls)["(Intercept)"]; a2 <- coef(dem_2sls)["ps"]; a3 <- coef(dem_2sls)["di"]
b0 <- coef(sup_2sls)["(Intercept)"]; b2 <- coef(sup_2sls)["pf"]
lambda1 <- coef(dem_2sls)["q"];      theta1 <- coef(sup_2sls)["q"]

Q_grid <- Q_grid %>% 
  mutate(P_d = a0 + lambda1*Q + a2*22 + a3*3.5,
         P_s = b0 + theta1*Q + b2*23)

ggplot(Q_grid, aes(x = Q)) +
  geom_line(aes(y = P_d), colour = "steelblue", size = 1.1) +
  geom_line(aes(y = P_s), colour = "darkred" , size = 1.1) +
  labs(title = "Truffle Market: Supply & Demand (P on vertical axis)",
       x = "Quantity (Q)", y = "Price (P)") +
  theme_minimal()

#11.28.e
num <- (b0 + b2*23) - (a0 + a2*22 + a3*3.5)
den <- lambda1 - theta1
Q_eq <- num / den
P_eq <- a0 + lambda1*Q_eq + a2*22 + a3*3.5
c(Q_eq = Q_eq, P_eq = P_eq)

rf_Q <- lm(q ~ ps + di + pf, data = truffles)
rf_P <- lm(p ~ ps + di + pf, data = truffles)
newX <- data.frame(ps = 22, di = 3.5, pf = 23)
Q_hat <- predict(rf_Q, newdata = newX)   # π̂11 + π̂21*22 + π̂31*3.5 + π̂41*23
P_hat <- predict(rf_P, newdata = newX)   # π̂12 + π̂22*22 + π̂32*3.5 + π̂42*23
c(Q_hat = Q_hat, P_hat = P_hat)

#11.28.f
dem_ols <- lm(p ~ q + ps + di, data = truffles)   # 需求 OLS
sup_ols <- lm(p ~ q + pf     , data = truffles)   # 供給 OLS

bind_rows(
  tidy(dem_ols) %>% mutate(model = "Demand OLS"),
  tidy(sup_ols) %>% mutate(model = "Supply OLS"),
  tidy(dem_2sls) %>% mutate(model = "Demand 2SLS"),
  tidy(sup_2sls) %>% mutate(model = "Supply 2SLS")
) %>%
  select(model, term, estimate, std.error, statistic, p.value) %>%
  arrange(term, model) %>%
  print(n = Inf)

#11.30.a
data("klein")
summary(klein)
library(car)   
library(lmtest)  
library(sandwich)

ols_inv <- lm(i ~ p + plag + klag, data = klein)
cat("\n(a)  OLS estimates of 11.18:\n")
summary(ols_inv)
print(tidy(ols_inv), n = Inf) 

#11.30.b
rf_P <- lm(p ~ g + w2 + tx + time + plag + klag + elag, data = klein)
summary(rf_P)


joint_test <- linearHypothesis(
  rf_P,
  c("g = 0", "w2 = 0", "tx = 0", "time = 0", "elag = 0")
)
cat("\nJoint F-test (g, w2, tx, time, elag):\n")
print(joint_test, digits = 3)

F_stat <- joint_test$F[2]       
df1    <- joint_test$Df[2]      
df2    <- joint_test$Res.Df[2]  
F_crit <- qf(0.95, df1, df2)   

cat("F statistic =", round(F_stat, 3), "\n")
cat("Critical F(5,13;0.95) =", round(F_crit, 3), "\n")

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

#11.30.c
hausman <- lm(i ~ p + plag + klag + vhat, data = df)
cat("\n(c)  Hausman test (t-stat of vhat):\n")
print(coeftest(hausman, vcov. = vcovHC(hausman, type = "HC1"))["vhat", ])
summary(hausman)

#11.30.d
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

#11.30.e
stage2 <- lm(i ~ phat + plag + klag, data = df)
cat("\n(e)  Second-stage OLS (I_t on p̂):\n")
summary(stage2)

#11.30.f
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
  cat("   → Fail to reject H0 : surplus instruments appear valid.\n")
} else {
  cat("   → Reject H0 : at least one surplus instrument may be invalid.\n")
}

#15.17.a
data("liquor5")
summary(liquor5)
liquor_fd <- liquor5 %>%
  group_by(hh) %>%      
  arrange(year) %>%               
  mutate(
    LIQUORD  = liquor  - lag(liquor),
    INCOMED  = income - lag(income)
  ) %>%
  filter(!is.na(liquor) & !is.na(income)) %>%
  ungroup()

fd_mod <- lm(LIQUORD ~ INCOMED - 1, data = liquor_fd)

summary(fd_mod)
confint(fd_mod, level = 0.95)

#15.20.a
data("star")
summary(star)
ols_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = star)
summary(ols_a)

#15.20.b
pdata <- pdata.frame(star, index = c("schid","id"))

fe_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
            data = pdata,
            model = "within")
summary(fe_b)

#15.20.c
pool_c <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
              data = pdata,
              model = "pooling")

pFtest(fe_b, pool_c)
