library(POE5Rdata)
library(dplyr)
library(AER)
library(car)
library(boot)
data <- POE5Rdata::mroz

#Q10.18.a
data <- data %>%
  mutate(
    mothercoll = ifelse(mothereduc > 12, 1, 0),
    fathercoll = ifelse(fathereduc > 12, 1, 0)
  )

data %>%
  summarise(
    prop_fathercoll = mean(fathercoll, na.rm = TRUE),
    prop_mothercoll = mean(mothercoll, na.rm = TRUE),
    prop_either_coll = mean(fathercoll == 1 | mothercoll == 1, na.rm = TRUE),
    prop_both_coll = mean(fathercoll == 1 & mothercoll == 1, na.rm = TRUE)
  )

#Q10.18.b
# Subset the relevant variables
vars <- data %>% select(educ, mothercoll, fathercoll)

# Compute correlation matrix
cor_matrix <- cor(vars, use = "complete.obs")
print(round(cor_matrix, 2), quote = FALSE)

#Q10.18.c
data_clean <- data %>%
  filter(!is.na(wage), wage > 0)
modiv <- ivreg(log(wage) ~ educ + exper + I(exper^2)| mothercoll + exper + I(exper^2), data=data_clean)
summary(modiv)

confiv <- confint(modiv,level = 0.95)
confiv

#Q10.18.d
modfirst <- lm(educ ~ mothercoll + exper + I(exper^2),data = data_clean)
summary(modfirst)
linearHypothesis(modfirst, "mothercoll = 0")

#Q10.18.e
modiv2 <- ivreg(log(wage) ~ educ + exper + I(exper^2)| mothercoll+ fathercoll + exper + I(exper^2), data=data_clean)
confiv2 <- confint(modiv2, level = 0.95)
confiv2

#Q10.18.f
modfirst2 <- lm(educ ~ mothercoll + fathercoll + exper + I(exper^2),data = data_clean)
summary(modfirst2)
linearHypothesis(modfirst2, c("mothercoll = 0", "fathercoll = 0"))

#Q10.18.g
modlm <- lm(log(wage) ~ educ + exper + I(exper^2), data = data_clean)
e <- resid(modlm)
modtest <- lm(e ~ mothercoll + fathercoll, data = data_clean)
nr2 <-(nrow(data_clean))*(summary(modtest)$r.squared)
chicrit <- qchisq(0.95,2-1)
cat(chicrit , nr2)

#Q10.20.a
data <- POE5Rdata::capm5
rp <- data %>%
  mutate(
    ge = ge - riskfree,
    ibm = ibm - riskfree,
    ford = ford - riskfree,
    msft = msft - riskfree,
    dis = dis - riskfree,
    xom = xom - riskfree,
    mkt = mkt - riskfree
  )

modlm <- lm(msft ~ mkt ,data = rp)
summary(modlm)

#Q10.20.b
rp_sort <- rp[order(rp$mkt),]
rp_sort <- rp_sort %>%
  mutate(rank = row_number())
modivrank <- ivreg(msft ~ mkt|rank , data = rp_sort)
summary(modivrank,diagnostics = TRUE)
mod1strank <- lm(mkt ~ rank, data = rp_sort)
summary(mod1strank)

#Q10.20.c
rp_sort <- rp_sort %>%
  mutate(v_hat = resid(mod1strank))
modresid = lm(msft ~ mkt + v_hat , data = rp_sort)
confresid <- confint(modresid, level = 0.99)
confresid

#Q10.20.e
rp_sort <- rp_sort %>%
  mutate(pos = ifelse(mkt>0,1,0))
mod1strankpos <- lm(mkt ~ rank+pos, data = rp_sort)
summary(mod1strankpos)
linearHypothesis(mod1strankpos, c("pos = 0", "rank = 0"))

#Q10.20.f
modivrankpos <- ivreg(msft ~ mkt|rank+pos , data = rp_sort)
summary(modivrankpos,diagnostics = TRUE)

#10.24.a
data <- POE5Rdata::mroz
data <- data %>%
  filter(!is.na(wage), wage > 0)
modiv <- ivreg (log(wage) ~ educ + exper + I(exper^2)| mothereduc + fathereduc + exper + I(exper^2) , data = data)
ehat = resid(modiv)
plot(data$exper , ehat , xlab = "EXPER", ylab = 'IV residual', main = 'Residual plot')

#10.24.b
bptest(modiv,
       varformula = ~ exper,
       data = data)

#10.24.c
robust_se <- vcovHC(modiv, type = "HC3")

beta_educ <- coef(modiv)["educ"]
se_educ_rob <- sqrt(robust_se["educ", "educ"])
se_educ <- sqrt(vcov(modiv)["educ", "educ"])

df <- nrow(data) - length(coef(modiv))
t_critical <- qt(0.975, df)

lower_bound_rob <- beta_educ - t_critical * se_educ_rob
upper_bound_rob <- beta_educ + t_critical * se_educ_rob
lower_bound <- beta_educ - t_critical * se_educ
upper_bound <- beta_educ + t_critical * se_educ

cat("95% CI for the coefficient of 'educ': [", lower_bound, ",", upper_bound, "]\n","95% CI for the coefficient of 'educ' with robust SE: [", lower_bound_rob, ",", upper_bound_rob, "]\n")

#10.24.d

bootstrap_function <- function(data, indices) {
  # 使用索引對數據進行重抽樣
  boot_data <- data[indices, ]
  
  # 對重抽樣數據進行 IV/2SLS 回歸
  mod <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothereduc + fathereduc + exper + I(exper^2), data = boot_data)
  
  # 返回 'educ' 變數的係數
  return(coef(mod)["educ"])
}

# 進行 200 次 bootstrap 重複抽樣
set.seed(123)  # 設定隨機種子以保證結果可重現
bootstrap_results <- boot(data = data, statistic = bootstrap_function, R = 200)

# 顯示 bootstrap 結果
print(bootstrap_results)

# 計算 'educ' 變數的 95% 置信區間
bootstrap_ci <- boot.ci(bootstrap_results, type = "perc", index = 1) # 使用 index=1，根據 coef(modiv) 中 educ 的位置

# 顯示 95% 置信區間
cat("Bootstrap 95% CI for the coefficient of 'educ':[", bootstrap_ci$percent[4], ',', bootstrap_ci$percent[5],"]", "\n")

# 計算異方差一致標準誤（參考 (c) 中的要求）
robust_se <- vcovHC(modiv, type = "HC3")
cat("Heteroskedasticity-robust standard error for EDUC: ", sqrt(robust_se["educ", "educ"]), "\n")
