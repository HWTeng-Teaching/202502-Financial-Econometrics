rm(list=ls())  
library(POE5Rdata)  
data("commute5") 

##Q31(a)


# 建立回歸模型
model <- lm(time ~ depart + reds + trains, data = commute5)

# 顯示回歸摘要（係數估計、t 值、p 值）
summary(model)

# 建立 95% 信賴區間
confint(model, level = 0.95)


new_data <- data.frame(depart = 30, reds = 6, trains = 1)
predict(model, newdata = new_data, interval = "confidence", level = 0.95)

qt(0.9, df = 245)  # ≈ 1.645




##Q33(a)
rm(list=ls())  
library(POE5Rdata)  
data("cps5_small") 


model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)
summary(model)



##(c)


# 擬合模型
model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)

# 顯示模型係數
summary(model)

# 抽取估計係數
b <- coef(model)
b2 <- b["educ"]
b3 <- b["I(educ^2)"]
b6 <- b["I(educ * exper)"]

# 計算每一觀察值的邊際效應
marginal_effect <- b2 + 2 * b3 * cps5_small$educ + b6 * cps5_small$exper

summary(marginal_effect)

# 畫出 histogram
hist(marginal_effect, breaks = 30, main = "Marginal Effect of educ on ln(wage)",
     xlab = "Marginal Effect", col = "lightblue", border = "white")

# 計算中位數、5th percentile 與 95th percentile
median_effect <- median(marginal_effect)
percentile_5 <- quantile(marginal_effect, 0.05)
percentile_95 <- quantile(marginal_effect, 0.95)

# 顯示結果
cat("Median marginal effect: ", median_effect, "\n")
cat("5th percentile: ", percentile_5, "\n")
cat("95th percentile: ", percentile_95, "\n")





##(e)
# 提取係數
b <- coef(model)
b4 <- b["exper"]
b5 <- b["I(exper^2)"]
b6 <- b["I(educ * exper)"]

# 計算 EXPER 對 ln(WAGE) 的邊際效應
marginal_effect <- b4 + 2 * b5 * cps5_small$exper + b6 * cps5_small$educ


# 畫直方圖
hist(marginal_effect, breaks = 30,
     main = "Marginal Effect of EXPER on ln(WAGE)",
     xlab = "Marginal Effect", col = "lightgreen", border = "white")

# 計算中位數、5th percentile 與 95th percentile
median_effect <- median(marginal_effect)
percentile_5 <- quantile(marginal_effect, 0.05)
percentile_95 <- quantile(marginal_effect, 0.95)

# 顯示結果
cat("Median marginal effect: ", median_effect, "\n")
cat("5th percentile: ", percentile_5, "\n")
cat("95th percentile: ", percentile_95, "\n")


##(f)

# 建立兩位的資料
david <- data.frame(educ = 17, exper = 8)
svetlana <- data.frame(educ = 16, exper = 18)

# 加入平方與交互作用項
david$educ2 <- david$educ^2
david$exper2 <- david$exper^2
david$interact <- david$educ * david$exper

svetlana$educ2 <- svetlana$educ^2
svetlana$exper2 <- svetlana$exper^2
svetlana$interact <- svetlana$educ * svetlana$exper

# 確保變數名稱跟模型一致（可選：依你模型的 formula 寫法）
names(david) <- c("educ", "exper", "I(educ^2)", "I(exper^2)", "I(educ * exper)")
names(svetlana) <- names(david)

# 計算預測的 log(wage)
pred_david <- predict(model, newdata = david, se.fit = TRUE)
pred_svetlana <- predict(model, newdata = svetlana, se.fit = TRUE)

# 計算 log(wage) 的差異與標準誤
diff <- pred_david$fit - pred_svetlana$fit
se_diff <- sqrt(pred_david$se.fit^2 + pred_svetlana$se.fit^2)

# t 值
t_stat <- diff / se_diff

# 右尾檢定 p 值（單尾檢定）
p_value <- pt(t_stat, df = model$df.residual, lower.tail = FALSE)

# 印出結果
cat("🔍 Difference in predicted log(WAGE):", diff, "\n")
cat("📏 Standard error of difference:", se_diff, "\n")
cat("🧪 t-statistic:", t_stat, "\n")
cat("📉 p-value:", p_value, "\n")


x_david <- c(1, 17, 17^2, 8, 8^2, 17*8)
x_svetlana <- c(1, 16, 16^2, 18, 18^2, 16*18)

x_diff <- as.matrix(x_david - x_svetlana)  # 向量差
vcov_mat <- vcov(model)

se_diff <- sqrt(t(x_diff) %*% vcov_mat %*% x_diff)

qt(0.05,1194)


##(g)
x_david_new <- c(1, 17, 289, 16, 256, 272)
x_svetlana_new <- c(1, 16, 256, 26, 676, 416)
x_diff <- as.matrix(x_david_new - x_svetlana_new)
vcov_mat <- vcov(model)

x_diff <- as.matrix(x_david - x_svetlana)  # 向量差
vcov_mat <- vcov(model)

se_diff <- sqrt(t(x_diff) %*% vcov_mat %*% x_diff)
qt(0.05,1194)


##(h)
L <- matrix(c(0, 0, 0, 0, 3, -1), nrow = 1)
beta_hat <- coef(model)
se2 <- L %*% vcov(model) %*% t(L)
se <- sqrt(se2)
estimate <- L %*% beta_hat
t_stat <- as.numeric(estimate / se)
p_value <- 2 * pt(-abs(t_stat), df = model$df.residual)
qt(0.975,1194)


##(i)
install.packages("car")
library(car)

deltaMethod(coef(model),"-(exper + 16 * (educ * exper)) / (2 * (exper))", vcov(model))


jill_se <- function(model) {
  b <- coef(model)
  V <- vcov(model)
  b3 <- b["exper"]; b4 <- b["I(exper^2)"]; b5 <- b["I(educ * exper)"]
  var_b3 <- V["exper", "exper"]
  var_b4 <- V["I(exper^2)", "I(exper^2)"]
  var_b5 <- V["I(educ * exper)", "I(educ * exper)"]
  cov_b3b4 <- V["exper", "I(exper^2)"]
  cov_b3b5 <- V["exper", "I(educ * exper)"]
  cov_b4b5 <- V["I(exper^2)", "I(educ * exper)"]
  
  term1 <- (-1 / (2 * b4))^2 * var_b3
  term2 <- ((b3 + 16 * b5) / (2 * b4^2))^2 * var_b4
  term3 <- (-16 / (2 * b4))^2 * var_b5
  term4 <- 2 * (-1 / (2 * b4)) * ((b3 + 16 * b5) / (2 * b4^2)) * cov_b3b4
  term5 <- 2 * (-1 / (2 * b4)) * (-16 / (2 * b4)) * cov_b3b5
  term6 <- 2 * ((b3 + 16 * b5) / (2 * b4^2)) * (-16 / (2 * b4)) * cov_b4b5
  
  sqrt(term1 + term2 + term3 + term4 + term5 + term6)
}

jill_se(model)
