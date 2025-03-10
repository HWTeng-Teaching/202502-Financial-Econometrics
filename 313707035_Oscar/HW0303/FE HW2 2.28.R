library(POE5Rdata)
data("cps5_small")
df <- cps5_small

# 確保 wage 和 educ 為數值型
df$wage <- as.numeric(as.character(df$wage))
df$educ <- as.numeric(as.character(df$educ))

# 確保性別與種族變數為數值型（僅當變數存在時轉換）
if ("male" %in% names(df)) {
  df$male <- as.numeric(as.character(df$male))
}
if ("female" %in% names(df)) {
  df$female <- as.numeric(as.character(df$female))
}
if ("black" %in% names(df)) {
  df$black <- as.numeric(as.character(df$black))
}
if ("white" %in% names(df)) {
  df$white <- as.numeric(as.character(df$white))
}

(a)
# 摘要統計
# 正確的程式碼
summary(df[, c("wage", "educ")])

# WAGE 直方圖
hist(df$wage, main="Histogram of wage", xlab="wage", col="lightblue")

# educ 直方圖
hist(df$educ, main="Histogram of educ", xlab="educ (Years of Education)", col="lightgreen")

# (b) 線性回歸
lm_linear <- lm(wage ~ educ, data=df)
summary(lm_linear)

# (c) 殘差分析
residuals_linear <- resid(lm_linear)
plot(df$educ, residuals_linear,
     xlab="Years of Education", ylab="Residuals",
     main="Residuals vs educ",
     col="purple", pch=16)
abline(h=0, col="red")

# (d) 依性別與種族區分
model_male <- lm(wage ~ educ, data = cps5_small, subset = (female == 0)) 
model_female <- lm(wage ~ educ, data = cps5_small, subset = (female == 1)) 
model_black <- lm(wage ~ educ, data = cps5_small, subset = (black == 1)) 
model_white <- lm(wage ~ educ, data = cps5_small, subset = (black == 0)) 

# 顯示回歸結果 
summary(model_male) 
summary(model_female) 
summary(model_black) 
summary(model_white)

# (e) 二次回歸模型

# 建立二次回歸模型
model_full_quad <- lm(wage ~ I(educ^2), data = cps5_small)
# 顯示回歸結果
summary(model_full_quad)

# 提取二次回歸模型的 educ^2 係數
alpha2 <- coef(model_full_quad)["I(educ^2)"]

# 計算邊際影響（Marginal Effect）在 12 年與 16 年教育時
ME_12 <- 2 * alpha2 * 12
ME_16 <- 2 * alpha2 * 16

# 顯示結果
cat("Marginal Effect at 12 years of education:", ME_12, "\n")
cat("Marginal Effect at 16 years of education:", ME_16, "\n")

# (f) 預測值比較
educ_range <- seq(min(df$educ), max(df$educ), by=0.1)

pred_linear <- predict(lm_linear, newdata=data.frame(educ=educ_range))
pred_quad <- predict(lm_quad, newdata=data.frame(educ=educ_range, educ2=educ_range^2))

plot(df$educ, df$wage, pch=20, col="gray",
     main="Linear vs Quadratic Regression",
     xlab="Years of Education", ylab="wage")

lines(educ_range, pred_linear, col="blue", lwd=2, lty=2)
lines(educ_range, pred_quad, col="red", lwd=2)

legend("topleft", legend=c("Linear", "Quadratic"),
       col=c("blue", "red"), lty=c(2,1), lwd=2)
