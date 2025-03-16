# # 建立線性回歸模型，包含性別變數
# model_gender <- lm(WAGE ~ EDUC + FEMALE, data = cps5_data)
# 
# # 顯示回歸結果
# summary(model_gender)
# 
# # 格式化顯示回歸方程式
# coef_values <- coef(model_gender)
# equation <- paste0("WAGE = ", round(coef_values[1], 4), 
#                    " + ", round(coef_values[2], 4), " * EDUC", 
#                    " + ", round(coef_values[3], 4), " * FEMALE")
# cat("性別回歸方程式：", equation, "\n")
# cat("截距 (β1)：", round(coef_values[1], 4), "\n")
# cat("教育斜率 (β2)：", round(coef_values[2], 4), "\n")
# cat("性別斜率 (β3)：", round(coef_values[3], 4), "\n")
# cat("R平方：", round(summary(model_gender)$r.squared, 4), "\n")



# # 建立僅含平方項的線性回歸模型
# model_quad_only <- lm(WAGE ~ I(EDUC^2), data = cps5_data)
# 
# # 顯示回歸結果
# summary(model_quad_only)
# 
# # 格式化顯示回歸方程式
# coef_values <- coef(model_quad_only)
# equation <- paste0("WAGE = ", round(coef_values[1], 4), 
#                    " + ", round(coef_values[2], 4), " * EDUC^2")
# cat("平方項回歸方程式：", equation, "\n")
# cat("截距 (α1)：", round(coef_values[1], 4), "\n")
# cat("平方項係數 (α2)：", round(coef_values[2], 4), "\n")
# cat("R平方：", round(summary(model_quad_only)$r.squared, 4), "\n")

