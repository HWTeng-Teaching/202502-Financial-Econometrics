# 安裝並載入必要的套件
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

# 載入數據集
library(POE5Rdata)
data("cps5_small")


# 繪製 FOODAWAY 的直方圖
library(ggplot2)
library(dplyr)

# ## a.
# # 計算WAGE與EDUC的摘要統計
# wage_summary <- summary(cps5_small$wage)
# educ_summary <- summary(cps5_small$educ)
# 
# cat("Summary Statistics for WAGE:\n")
# print(wage_summary)
# 
# cat("\nSummary Statistics for EDUC:\n")
# print(educ_summary)
# 
# # 繪製 WAGE 的直方圖
# print(
# ggplot(cps5_small, aes(x = wage)) +
#   geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
#   labs(title = "Histogram of WAGE", x = "WAGE", y = "Frequency") +
#   theme_minimal()
# )
# # 繪製 EDUC 的直方圖
# print(
# ggplot(cps5_small, aes(x = educ)) +
#   geom_histogram(binwidth = 1, fill = "lightgreen", color = "black", alpha = 0.7) +
#   labs(title = "Histogram of EDUC", x = "EDUC", y = "Frequency") +
#   theme_minimal()
# )

# ## b.
# # Fit the linear regression model
# model <- lm(wage ~ educ, data = cps5_small)
# results <- list()
# results$summary <- summary(model)
# 
# # Display the summary of the model
# summary(model)
# beta_1 <- coef(model)[1]
# beta_2 <- coef(model)[2]
# 
# cat("截距",beta_1,"\n")
# cat("斜率",beta_2,"\n")
# print(results$summary)

# ## c.
# # 擬合線性回歸模型
# model <- lm(wage ~ educ, data = cps5_small)
# 
# # 計算殘差
# residuals <- residuals(model)
# 
# # 繪製殘差與EDUC的關係圖
# print(
#   ggplot(cps5_small, aes(x = educ, y = residuals)) +
#     geom_point(color = "blue", alpha = 0.6) +
#     labs(title = "殘差與EDUC的關係圖",
#          x = "教育年限 (EDUC)",
#          y = "殘差") +
#     theme_minimal()
#   )

## d.
# 分別為男性、女性、黑人和白人進行回歸分析
model_male <- lm(wage ~ educ, data = filter(cps5_small, female == 0))
model_female <- lm(wage ~ educ, data = filter(cps5_small, female == 1))
model_black <- lm(wage ~ educ, data = filter(cps5_small, black == 1))
model_white <- lm(wage ~ educ, data = filter(cps5_small, black == 0))

# 顯示每個回歸模型的摘要
cat("男性回歸結果:\n")
print(summary(model_male))

cat("\n女性回歸結果:\n")
print(summary(model_female))

cat("\n黑人回歸結果:\n")
print(summary(model_black))

cat("\n白人回歸結果:\n")
print(summary(model_white))



