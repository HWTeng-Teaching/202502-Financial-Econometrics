url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/truffles.rdata"
file_path <- "truffles.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(truffles)
#install.packages("systemfit")
library(systemfit)

#b
eq_d <- p ~ q + ps + di     
eq_s <- p ~ q + pf           
inst <- ~ ps + di + pf       

sys2sls <- systemfit(list(demand = eq_d,
                          supply = eq_s),
                     method = "2SLS",
                     inst   = inst,
                     data   = truffles)
summary(sys2sls)

#c
library(AER)

dem_2sls <- ivreg(p ~ q + ps + di | ps + di + pf, data = truffles) 
sup_2sls <- ivreg(p ~ q + pf      | ps + di + pf, data = truffles)
lambda1 <- coef(dem_2sls)["q"]
Pbar <- mean(truffles$p)
Qbar <- mean(truffles$q)

elas <- (1/lambda1) * (Pbar / Qbar)
elas

#d
#install.packages("tibble")       # 如果沒裝過
library(tibble)                  # 每次使用前先載入

library(dplyr)                # 每次使用前先載入
library(ggplot2)
Q_grid <- tibble(Q = seq(min(truffles$q)*0.9, max(truffles$q)*1.1, length.out = 100))

a0 <- coef(dem_2sls)["(Intercept)"]; a2 <- coef(dem_2sls)["ps"]; a3 <- coef(dem_2sls)["di"]
b0 <- coef(sup_2sls)["(Intercept)"]; b2 <- coef(sup_2sls)["pf"]
lambda1 <- coef(dem_2sls)["q"];      theta1 <- coef(sup_2sls)["q"]

Q_grid <- Q_grid %>% 
  mutate(P_d = a0 + lambda1*Q + a2*22 + a3*3.5,
         P_s = b0 + theta1*Q + b2*23)

ggplot(Q_grid, aes(x = Q)) +
  geom_line(aes(y = P_d), colour = "blue", size = 1.1) +
  geom_line(aes(y = P_s), colour = "red" , size = 1.1) +
  labs(title = "Truffle Market: Supply & Demand (P on vertical axis)",
       x = "Quantity (Q)", y = "Price (P)") +
  theme_minimal()


#e
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


#f
#install.packages("broom")
library(broom)

dem_ols <- lm(p ~ q + ps + di, data = truffles)   # 需求 OLS
sup_ols <- lm(p ~ q + pf     , data = truffles)   # 供給 OLS

summary(dem_ols)
summary(sup_ols)

bind_rows(
  tidy(dem_ols) %>% mutate(model = "Demand OLS"),
  tidy(sup_ols) %>% mutate(model = "Supply OLS"),
  tidy(dem_2sls) %>% mutate(model = "Demand 2SLS"),
  tidy(sup_2sls) %>% mutate(model = "Supply 2SLS")
) %>%
  select(model, term, estimate, std.error, statistic, p.value) %>%
  arrange(term, model) %>%
  print(n = Inf)






































