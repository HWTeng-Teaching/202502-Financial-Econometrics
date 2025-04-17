rm(list = ls()) 
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
vacation

# a.

model1 <- lm(miles~income+age+kids, vacation)
smodel1 <- summary(model1)
smodel1
b4 <- as.numeric(coef(model1)["kids"])
ci <- confint(model1, "kids", level = 0.95)
lowerci <- ci["kids", "2.5 %"]
upperci <- ci["kids", "97.5 %"]
cat("normal 95% CI = [", lowerci,upperci, "]")

# b.

resid1 <- resid(model1)
ggplot(data = vacation, aes(x = income, y = resid1))+
  geom_point(color = "blue")+
  labs(title = "residual plot on income",
       x = "income",
       y = "residual"
       )
# 具有異質變異數，因為隨著收入增加殘差的範圍越來越大。
ggplot(data = vacation, aes(x = age, y = resid1))+
  geom_point(color = "blue")+
  labs(title = "residual plot on age",
       x = "age",
       y = "residual"
  )
#看起來分佈平均，殘差沒有隨著 age 的趨勢變動。 

# c.

vacation_sorted <- vacation %>% arrange(income)
# 把中間20筆刪除
n <- nrow(vacation_sorted)
drop_n <- 20
group_n <- (n - drop_n) / 2
group1 <- vacation_sorted[1:group_n, ]
group2 <- vacation_sorted[(n - group_n + 1):n, ]

model2 <- lm(miles ~ income + age + kids, data = group1)
model3 <- lm(miles ~ income + age + kids, data = group2)
SSE2 <- sum(resid(model2)^2)
SSE3 <- sum(resid(model3)^2)

F_stat <- max(SSE2, SSE3) / min(SSE2, SSE3)
df2 <- model2$df.residual
df3 <- model3$df.residual
p_value <- pf(F_stat, df2, df3, lower.tail = FALSE)

cat("Goldfeld–Quandt Test 結果：\n")
cat("F 統計量 =", F_stat, "\n")
cat("自由度 =", df2, "和", df3, "\n")
cat("p 值 =", p_value, "\n")
if (p_value < 0.05) {
  cat("結論：拒絕 H0，存在異質變異（heteroskedasticity）\n")
} else {
  cat("結論：無法拒絕 H0，沒有足夠證據顯示存在異質變異\n")
}
# H0:模型不存在異質變異數 vs. H1：模型存在異質變異數

# d.

cov1 <- hccm(model1, type = "hc1")
vcvmodel1 <- coeftest(model1, vcov = cov1)
vcvmodel1
robust_var <- vcvmodel1["kids", "Std. Error"]
robust_se <- sqrt(robust_var)
robust_var
robust_se

alpha <- 0.05
z <- qt(1 - alpha/2, df = model1$df.residual) 
lower <- b4 - z * robust_se
upper <- b4 + z * robust_se
lower
cat("95% CI =","[", lower, ",", upper, "]")
# 利用 robust 標準誤的區間估計範圍較窄

# e.

gls_model <- gls(miles ~ income + age + kids, data = vacation,
                 weights = varFixed(~ income^2))
summary(gls_model)

coefs <- summary(gls_model)$tTable
kids_est <- coefs["kids", "Value"]
kids_se <- coefs["kids", "Std.Error"]

df <- gls_model$dims$N - gls_model$dims$p
t_crit <- qt(0.975, df)

ci_lower <- kids_est - t_crit * kids_se
ci_upper <- kids_est + t_crit * kids_se
cat("Conventional GLS 95% CI = [", ci_lower, ",", ci_upper, "]")
cat("Robust GLS 95% CI =","[", lower, ",", upper, "]")
cat("normal 95% CI = [", lowerci,upperci, "]")

diffcgls <- ci_upper - ci_lower
diffrgls <- upper - lower
diffnorm <- upperci - lowerci

diff_table <- data.frame(
  Conventional_GLS = round(diffcgls, digits = 4),
  Robust_GLS       = round(diffrgls, digits = 4),
  OLS              = round(diffnorm, digits = 4)
)

print(diff_table)
# 利用 robust 標準誤的模型區間估計最窄，較為精確，傳統 OLS 範圍最大



