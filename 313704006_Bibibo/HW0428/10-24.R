library(POE5Rdata)
data("mroz")
?mroz

married_data = mroz[mroz$lfp==1, ]
#a
mroz.iv = ivreg(log(wage)~educ+exper+I(exper^2)|
                  exper+I(exper^2)+mothereduc+fathereduc, data = married_data)
#y_hat = exp(fitted(mroz.iv))
#residual_hat = married_data$wage - y_hat
#plot(married_data$exper, residual_hat)
vcov_mroz = vcov(mroz.iv)
residual_iv <- resid(mroz.iv)
plot(married_data$exper, residual_iv)

#b
re2 = residual_iv^2
model = lm(re2~exper, data = married_data)
R2 = summary(model)$r.squared
N = nrow(married_data)
NR2 = N*R2 #7.4385
p_value = 1 - pchisq(NR2, df = 1) #0.00638
#reject H0, heteroskedasticity

#c
mroz.iv_robust = ivreg(log(wage)~educ+exper+I(exper^2)|
                  exper+I(exper^2)+mothereduc+fathereduc, data = married_data)
mroz.iv_robust$coefficients
vcov_mroz_robust <- vcov(mroz.iv, type = "HC1")
df = mroz.iv_robust$df.residual
tc = qt(0.975, df)
base_lb = mroz.iv$coefficients[2] - tc*(vcov_mroz[2, 2]^0.5)
base_ub = mroz.iv$coefficients[2] + tc*(vcov_mroz[2, 2]^0.5)
lb = mroz.iv_robust$coefficients[2] - tc*(vcov_mroz_robust[2, 2]^0.5)
ub = mroz.iv_robust$coefficients[2] + tc*(vcov_mroz_robust[2, 2]^0.5)
if((vcov_mroz_robust[2, 2]^0.5) >(vcov_mroz[2, 2]^0.5)){
  print("robust SE is larger than baseline")
}else{
  print("robust SE is smaller than baseline")
}
cat("95% CI for educ (robust SE): [", round(lb, 5), ",", round(ub, 5), "]\n")

#d
library(boot)
#放回抽樣
boot_iv <- function(data, indices) {
  sample_data <- data[indices, ]
  fit <- ivreg(log(wage) ~ educ + exper + I(exper^2) |
                 exper + I(exper^2) + mothereduc + fathereduc, 
               data = sample_data)
  return(coef(fit)["educ"])  # 我們只取 educ 的係數
}

set.seed(123)  # 可重複性
boot_results <- boot(data = married_data, statistic = boot_iv, R = 200)
#抽樣的資料, function 名稱, 重複幾次

boot_hat = mean(boot_results$t)
ci_boot = quantile(boot_results$t, c(0.025, 0.975))
boot_se = sd(boot_results$t)
cat("Bootstrap SE for educ coefficient:", round(boot_se, 5), "\n")
if(boot_se > (vcov_mroz[2, 2]^0.5)){
  print("wider than baseline")
}else{
  print("narrower than baseline")
}
if(boot_se > (vcov_mroz_robust[2, 2]^0.5)){
  print("wider than robust")
}else{
  print("narrower than robust")
}

# 整理資料框
library(ggplot2)
if(false){
  ci_df <- data.frame(
    Method = c("Baseline", "Robust", "Bootstrap"),
    Lower = c(base_lb, lb, ci_boot[1]),
    Upper = c(base_ub, ub, ci_boot[2]),
    Point = c(mroz.iv$coefficients[2], 
              mroz.iv_robust$coefficients[2], 
              boot_hat)
  )
  
  # 畫圖
  ggplot(ci_df, aes(x = Method, y = Point)) +
    geom_point(size = 3) +  # 點標示估計值
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +  # 設定誤差條 (信賴區間)
    ylab("Coefficient of educ") +
    ggtitle("95% Confidence Intervals for educ by Method") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}






