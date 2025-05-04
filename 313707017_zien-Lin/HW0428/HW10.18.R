# 載入所需的套件和數據集
library(POE5Rdata)
data("mroz")

# a.--------------------------
# 創建虛擬變數：mothercoll 和 fathercoll
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)

# 創建新變量：至少有一位父母有大學教育
mroz$at_least_one_parent_coll <- ifelse(mroz$mothercoll == 1 | mroz$fathercoll == 1, 1, 0)

# 計算百分比
percentage <- mean(mroz$at_least_one_parent_coll) * 100

# 輸出結果
cat("父母中至少有一方受過大學教育的百分比：", round(percentage, 2), "%\n")

# b.----------------------------
# 計算相關係數
cor_educ_mothercoll <- cor(mroz$educ, mroz$mothercoll)
cor_educ_fathercoll <- cor(mroz$educ, mroz$fathercoll)
cor_mothercoll_fathercoll <- cor(mroz$mothercoll, mroz$fathercoll)

# 輸出相關係數
cat("educ 與 mothercoll 的相關係數：", round(cor_educ_mothercoll, 3), "\n")
cat("educ 與 fathercoll 的相關係數：", round(cor_educ_fathercoll, 3), "\n")
cat("mothercoll 與 fathercoll 的相關係數：", round(cor_mothercoll_fathercoll, 3), "\n")

# C.------------------------------
install.packages("AER")
library(AER)  # 用於工具變量回歸

# 創建虛擬變數：mothercoll 和 fathercoll
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)

# 使用工具變量法估計工資方程
# 模型：lwage ~ educ，工具變量為 mothercoll 和 fathercoll
iv_model <- ivreg(wage ~ educ | mothercoll, data = mroz)

# 提取回歸結果
summary_iv <- summary(iv_model)

# 提取 educ 的係數和標準誤
educ_coef <- coef(summary_iv)["educ", "Estimate"]
educ_se <- coef(summary_iv)["educ", "Std. Error"]

# 計算 95% 置信區間
ci_lower <- educ_coef - 1.96 * educ_se
ci_upper <- educ_coef + 1.96 * educ_se

# 輸出結果
cat("educ 的係數估計：", round(educ_coef, 3), "\n")
cat("educ 係數的 95% 置信區間：(", round(ci_lower, 3), ", ", round(ci_upper, 3), ")\n")

# d.--------------------------
# 創建虛擬變數：mothercoll
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)

# 第一階段回歸：educ ~ mothercoll
first_stage <- lm(educ ~ mothercoll, data = mroz)

# 輸出第一階段回歸結果
summary_first_stage <- summary(first_stage)
cat("第一階段回歸結果：\n")
print(summary_first_stage)

# 擬合受限模型：educ ~ 1（僅包含截距）
restricted_model <- lm(educ ~ 1, data = mroz)

# 計算 F 檢驗統計量（檢驗 mothercoll 的係數是否為0）
f_test <- anova(restricted_model, first_stage)

# 提取 F 統計量
f_statistic <- f_test$F[2]

# 輸出 F 統計量
cat("假設 mothercoll 對 educ 無影響的 F 檢驗統計量：", round(f_statistic, 3), "\n")

# 判斷是否為強工具變量
if (f_statistic > 10) {
  cat("F 統計量 > 10，mothercoll 可能是強工具變量。\n")
} else {
  cat("F 統計量 <= 10，mothercoll 可能是弱工具變量。\n")
}

# e.------------------------
library(AER)  # 用於工具變量回歸

# 創建虛擬變數：mothercoll 和 fathercoll
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)

# 使用工具變量法估計工資方程
# 模型：lwage ~ educ，工具變量為 mothercoll 和 fathercoll
iv_model <- ivreg(wage ~ educ | mothercoll + fathercoll, data = mroz)

# 提取回歸結果
summary_iv <- summary(iv_model)

# 提取 educ 的係數和標準誤
educ_coef <- coef(summary_iv)["educ", "Estimate"]
educ_se <- coef(summary_iv)["educ", "Std. Error"]

# 計算 95% 置信區間
ci_lower <- educ_coef - 1.96 * educ_se
ci_upper <- educ_coef + 1.96 * educ_se

# 輸出結果
cat("educ 的係數估計：", round(educ_coef, 3), "\n")
cat("educ 係數的 95% 置信區間：(", round(ci_lower, 3), ", ", round(ci_upper, 3), ")\n")

# f.---------------------
# 創建虛擬變數：mothercoll 和 fathercoll
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)

# 第一階段回歸：educ ~ mothercoll + fathercoll
first_stage <- lm(educ ~ mothercoll + fathercoll, data = mroz)

# 輸出第一階段回歸結果
summary_first_stage <- summary(first_stage)
cat("第一階段回歸結果：\n")
print(summary_first_stage)

# 擬合受限模型：educ ~ fathercoll（排除 mothercoll）
restricted_model <- lm(educ ~ fathercoll, data = mroz)

# 計算 F 檢驗統計量（檢驗 mothercoll 的係數是否為0）
# 使用 anova() 比較完整模型和受限模型
f_test <- anova(restricted_model, first_stage)

# 提取 F 統計量
f_statistic <- f_test$F[2]

# 輸出 F 統計量
cat("假設 mothercoll 對 educ 無影響的 F 檢驗統計量：", round(f_statistic, 3), "\n")

# 判斷是否為強工具變量
if (f_statistic > 10) {
  cat("F 統計量 > 10，mothercoll 可能是強工具變量。\n")
} else {
  cat("F 統計量 <= 10，mothercoll 可能是弱工具變量。\n")
}