if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("AER")
install.packages("car")
library(POE5Rdata)
data('mroz')
#(a) 算出父母學歷百分比
# 建立新變數：MOTHERCOLL 與 FATHERCOLL
mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc > 12, 1, 0)
# 計算父母其中一人有大學教育以上的比例
parent_somecollege <- mean(mroz$mothercoll == 1 | mroz$fathercoll == 1)

# 轉為百分比格式
percentage <- parent_somecollege * 100

# 顯示結果
cat("Percentage of parents with some college education:", round(percentage, 2), "%\n")
# (b) 找教育程度相關性
subset_data <- mroz[, c("educ", "mothercoll", "fathercoll")]
cor_matrix <- cor(subset_data)
print(cor_matrix)
#(c)使用工具變數MOTHERCOLL跑2SLS
library(AER)
# IV regression：使用 MOTHERCOLL 為 EDUC 的工具變數(排除薪資為0，因log(0)不可解釋)
iv_model <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                    exper + I(exper^2) + mothercoll,
                  data = subset(mroz, wage > 0))
# 顯示摘要結果
summary(iv_model)
# 取出 EDUC 的 95% 信賴區間
confint(iv_model, level = 0.95)["educ", ]
# (d) first stage 回歸的F test
first_stage <- lm(educ ~ exper + I(exper^2) + mothercoll, data = mroz)
summary(first_stage)
# F 統計量：檢定 mothercoll 的係數是否為 0
library(car)
linearHypothesis(first_stage, "mothercoll = 0")
#(e)使用工具變數MOTHERCOLL、FATHERCOLL跑2SLS
library(AER)
iv_model_2 <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                    exper + I(exper^2) + mothercoll + fathercoll,
                  data = subset(mroz, wage > 0))
# 顯示摘要結果
summary(iv_model_2)
# 取出 EDUC 的 95% 信賴區間
confint(iv_model_2, level = 0.95)["educ", ]
#(f) e的first stage的F test(joint)
first_stage_2 <- lm(educ ~ exper + I(exper^2) + mothercoll + fathercoll, data = mroz)

# 回歸摘要
summary(first_stage_2)

# 聯合 F 檢定：檢定 mothercoll = 0 和 fathercoll = 0
linearHypothesis(first_stage_2, c("mothercoll = 0", "fathercoll = 0"))


