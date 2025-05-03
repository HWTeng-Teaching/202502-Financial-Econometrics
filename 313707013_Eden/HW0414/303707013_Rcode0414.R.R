library(POE5Rdata)
#8.16
data("vacation")
str(vacation)
#a
m1<-lm(miles~income+age+kids,data=vacation)
summary(m1)
confint(m1, level = 0.95)["kids", ]
#b
residuals_miles <- resid(m1)
# 畫殘差對 INCOME 的圖
plot(vacation$income, residuals_miles,
     main = "Residuals vs INCOME",
     xlab = "INCOME",
     ylab = "Residuals",
     pch = 19, col = "blue")
abline(h = 0, lty = 2)

plot(vacation$age, residuals_miles,
     main = "Residuals vs AGE",
     xlab = "AGE",
     ylab = "Residuals",
     pch = 19, col = "darkgreen")
abline(h = 0, lty = 2)
#c
# 排序資料
income_sorted <- vacation[order(vacation$income), ]
income_sorted
# 分組
group_low <- income_sorted[1:90, ]
group_high <- income_sorted[(nrow(income_sorted)-89):nrow(income_sorted), ]
# 對兩組分別做 OLS
model_low <- lm(miles~income+age+kids, data = group_low)
model_high <- lm(miles~income+age+kids, data = group_high)
# 計算 SSE（殘差平方和）
SSE_low <- sum(resid(model_low)^2)
SSE_high <- sum(resid(model_high)^2)
# 計算自由度
df_low <- nrow(group_low) - length(coef(model_low))
df_high <- nrow(group_high) - length(coef(model_high))
# 計算 F 統計量（高收入組在分子）
F_stat <- (SSE_high / df_high) / (SSE_low / df_low)


# 計算臨界值(單尾檢定)
alpha <- 0.05
F_crit <- qf(1 - alpha, df_high, df_low)
# 印出結果
cat("F 統計量 =", F_stat, "\n")
cat("臨界值 =", F_crit, "\n")
if (F_stat > F_crit) {
  cat("結論：拒絕 H0，存在異質變異性。\n")
} else {
  cat("結論：無法拒絕 H0，沒有足夠證據認為存在異質變異性。\n")
}


#d
install.packages("car")
install.packages("lmtest")
# 載入套件
library(car)
library(lmtest)
# 取得 heteroskedasticity-consistent covariance matrix（robust SE）
robust_cov <- hccm(m1,type = "hc1" )
m1_HC1 <-  coeftest(m1,vcov.=robust_cov) 
confint(m1_HC1, level = 0.95)["kids", ]
m1_HC1

#e
w <-1/vacation$income^2
gls_model <-lm(miles~income+age+kids,weight=w, data=vacation)
confint(gls_model1, level = 0.95)["kids", ]

robust_cov_gls <- hccm(gls_model,type = "hc1" )
gls_model_HC1 <-  coeftest(gls_model,vcov.=robust_cov_gls) 
confint(gls_model_HC1, level = 0.95)["kids", ]
gls_model_HC1

#8.18
data("cps5")

formula <-lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
              metro + south + midwest + west, data = cps5)
# 分割資料：male = female==0, female = female==1
data_male <- subset(cps5, female == 0)
data_female <- subset(cps5, female == 1)
# 回歸分析
model_male <- lm(formula, data = data_male)
model_female <- lm(formula, data = data_female)
# 取得 RSS 與自由度
rss_male <- sum(resid(model_male)^2)
rss_female <- sum(resid(model_female)^2)
df_male <- df.residual(model_male)
df_female <- df.residual(model_female)
# 計算 F 統計量
F_stat <- (rss_male / df_male)/(rss_female / df_female)
# 計算臨界值
alpha <- 0.05
F_crit_up <- qf(1-alpha/2, df_male, df_female)
F_crit_low <- qf(alpha/2, df_male, df_female)
cat("Goldfeld–Quandt Test\n")
cat("F 統計量:", round(F_stat, 4), "\n")
cat("臨界值 (5%):",round(F_crit_low,4),"&", round(F_crit_up, 4), "\n")

#b
cps5$resid_sq <- resid(formula)^2
aux_model1 <- lm(resid_sq ~ metro + female + black, data = cps5)
Rsq_1 <- summary(aux_model1)$r.squared
n <- nrow(cps5)
NRsq_1 <- n * Rsq_1
df1 <- 3  
crit_qchi_1 <- qchisq(0.99, df1)
cat("NR² test statistic (selected variables):", NRsq_1, "\n")
cat("Critical value (1% level, df = 3):", crit_qchi_1, "\n")

aux_model2 <- lm(resid_sq ~ educ + exper + I(exper^2) + female + black + 
                   metro + south + midwest + west, data = cps5)
Rsq_2 <- summary(aux_model2)$r.squared
df2 <- 9
NRsq_2 <- n * Rsq_2
crit_qchi_2 <- qchisq(0.99, df2)
cat("\nNR² test statistic (all variables):", NRsq_2, "\n")
cat("Critical value (1% level, df = 9):", crit_qchi_2, "\n")

#c
library(lmtest)  # 提供 bptest() 函數（White 檢定）
# White 檢定，輔助回歸包含原始變數、平方項和所有交互項
white_test <- bptest(formula,~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west +
    I(educ^2) + I(exper^4) + 
    educ:exper + educ:female + educ:black + educ:metro + educ:south + educ:midwest + educ:west + educ:I(exper^2) +
    exper:female + exper:black + exper:metro + exper:south + exper:midwest + exper:west + exper:I(exper^2) +
    female:black + female:metro + female:south + female:midwest + female:west + female:I(exper^2) + 
    black:metro + black:south + black:midwest + black:west + black:I(exper^2) + 
    metro:south + metro:midwest + metro:west + metro:I(exper^2) +
    south:I(exper^2) + midwest:I(exper^2) + west:I(exper^2),data = cps5)
# 臨界值（5%顯著水準）
df_white <- white_test$parameter  # 使用 bptest 中的自由度
critical_value_white <- qchisq(1-0.05, df_white)
cat("White 檢定 NR^2:", white_test$statistic, "\n")
cat("自由度:", white_test$parameter, "\n")
cat("5% 顯著水準的臨界值:", critical_value_white, "\n")

#d
model_ols <-lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
              metro + south + midwest + west, data = cps5)
cov1<-hccm(model_ols, type ="hc1")
model_hccm<-coeftest(model_ols, vcov.=cov1)
print(model_hccm)
summary(model_ols)

#e
model_ols <-lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
              metro + south + midwest + west, data = cps5)
ehatsq <- resid(model_ols)^2
sightsq.ols <-lm(log(ehatsq) ~ metro + exper, data = cps5)
sightsq.ols
var<- exp(fitted(sightsq.ols))
model_fgls<-lm(log(wage) ~ educ + exper + I(exper^2) + female + black + 
              metro + south + midwest + west,weight=1/var, data = cps5)
summary(model_fgls)
library(dplyr)
confint_robust <- cbind(
  Lower = coef(model_ols) - 1.96 * sqrt(diag(vcovHC(model_ols, type = "HC1"))),
  Upper = coef(model_ols) + 1.96 * sqrt(diag(vcovHC(model_ols, type = "HC1")))
)
confint_fgls <- confint(model_fgls)

comparison_df2 <- data.frame(
  FGLS_Lower = confint_fgls[, 1],
  FGLS_Upper = confint_fgls[, 2],
  Robust_Lower = confint_robust[, "Lower"],
  Robust_Upper = confint_robust[, "Upper"]
)
comparison_df2$FGLS_Width <- comparison_df2$FGLS_Upper - comparison_df2$FGLS_Lower
comparison_df2$Robust_Width <- comparison_df2$Robust_Upper - comparison_df2$Robust_Lower
comparison_df2$Change <- ifelse(comparison_df2$FGLS_Width > comparison_df2$Robust_Width, "Wider", "Narrower")

print(comparison_df2)

#f
robust_fgls <- coeftest(model_fgls, vcov. = vcovHC(model_fgls, type = "HC1"))
robust_fgls
robust_ci_fgls <- cbind(
  Estimate = robust_fgls[, 1],
  Lower = robust_fgls[, 1] - 1.96 * robust_fgls[, 2],
  Upper = robust_fgls[, 1] + 1.96 * robust_fgls[, 2]
)

robust_ols_ci <- cbind(
  Lower = coef(model_ols) - 1.96 * sqrt(diag(vcovHC(model_ols, type = "HC1"))),
  Upper = coef(model_ols) + 1.96 * sqrt(diag(vcovHC(model_ols, type = "HC1")))
)
fgls_ci <- confint(model_fgls)
comparison_df3 <- data.frame(
  RobustOLS_Lower = robust_ols_ci[, 1],
  RobustOLS_Upper = robust_ols_ci[, 2],
  FGLSRobust_Lower = robust_ci_fgls[, "Lower"],
  FGLSRobust_Upper = robust_ci_fgls[, "Upper"]
)

comparison_df3$OLSRobust_Width <- comparison_df3$RobustOLS_Upper - comparison_df3$RobustOLS_Lower
comparison_df3$FGLSRobust_Width <- comparison_df3$FGLSRobust_Upper - comparison_df3$FGLSRobust_Lower
comparison_df3$Change <- ifelse(comparison_df3$FGLSRobust_Width > comparison_df3$OLSRobust_Width, "Wider", "Narrower")

print(comparison_df3)

comparison_df4 <- data.frame(
  FGLS_Lower = confint_fgls[, 1],
  FGLS_Upper = confint_fgls[, 2],
  FGLSRobust_Lower = robust_ci_fgls[, "Lower"],
  FGLSRobust_Upper = robust_ci_fgls[, "Upper"]
)

comparison_df4$FGLS_Width <- comparison_df4$FGLS_Upper - comparison_df4$FGLS_Lower
comparison_df4$FGLSRobust_Width <- comparison_df3$FGLSRobust_Upper - comparison_df3$FGLSRobust_Lower
comparison_df4$Change <- ifelse(comparison_df4$FGLSRobust_Width > comparison_df4$FGLS_Width, "Wider", "Narrower")
print(comparison_df4)

