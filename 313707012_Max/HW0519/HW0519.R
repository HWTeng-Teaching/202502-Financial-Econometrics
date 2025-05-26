library(POE5Rdata)
data("liquor5")

#15.17
#a.------------------------------------------------------
# Sort data by hh (household) and year to ensure proper differencing
liquir <- liquor5[order(liquor5$hh, liquor5$year), ]

# Create first-differenced variables LIQUORD and INCOMED
liquir$LIQUORD <- ave(liquir$liquor, liquir$hh, 
                      FUN = function(x) c(NA, diff(x)))
liquir$INCOMED <- ave(liquir$income, liquir$hh, 
                      FUN = function(x) c(NA, diff(x)))

# Remove NA rows (first year for each household after differencing)
liquir <- liquir[!is.na(liquir$LIQUORD), ]

# Run OLS regression of LIQUORD on INCOMED with a constant
model <- lm(LIQUORD ~ INCOMED, data = liquir)

# Display regression summary
summary(model)

# Compute 95% confidence interval for the coefficient of INCOMED
conf_interval <- confint(model, "INCOMED", level = 0.95)
conf_interval

#b.------------------------------------------------------
# 載入 plm 套件以進行隨機效應估計
library(plm)

# 確保數據按家庭 (hh) 和年份 (year) 排序
liquor5 <- liquor5[order(liquor5$hh, liquor5$year), ]

# 估計隨機效應模型
model_b <- plm(liquor ~ income, 
               data = liquor5, 
               index = c("hh", "year"), 
               model = "random")

# 顯示隨機效應模型結果
summary(model_b)

# 計算 INCOME 係數的 95% 信賴區間
conf_interval_b <- confint(model_b, "income", level = 0.95)
cat("\n95% Confidence Interval for INCOME (Random Effects):\n")
print(conf_interval_b)

#c.------------------------------------------------------
# 執行 Breusch-Pagan (LM) 檢驗以測試隨機效應
lm_test <- plmtest(model_b, type = "bp")
cat("\nBreusch-Pagan LM Test for Random Effects:\n")
print(lm_test)

#d.------------------------------------------------------
# 載入 dplyr 以計算時間平均值
library(dplyr)

# 計算每個家庭的 INCOME 時間平均值 (INCOMEM)
liquor5 <- liquor5 %>%
  group_by(hh) %>%
  mutate(INCOMEM = mean(income, na.rm = TRUE))

# 估計 Mundlak 模型（隨機效應，包含 INCOMEM）
model_d <- plm(liquor ~ income + INCOMEM, 
               data = liquor5, 
               index = c("hh", "year"), 
               model = "random")

# 顯示 Mundlak 模型結果
summary(model_d)

# 檢驗 INCOMEM 係數 (γ) 的顯著性
cat("\nTest for significance of INCOMEM (γ):\n")
gamma_coef <- coef(summary(model_d))["INCOMEM", ]
cat(sprintf("Coefficient of INCOMEM: %.3f, p-value: %.3f\n", 
            gamma_coef["Estimate"], gamma_coef["Pr(>|t|)"]))

#15.20
#a.------------------------------------------------------
# 載入資料
library(POE5Rdata)
data("star")

model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)

# 顯示回歸結果
summary(model_a)

#b.------------------------------------------------------
# 加入學校固定效應
model_b <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch + factor(schid), data = star)

# 顯示回歸結果
summary(model_b)

# 比較兩個模型的係數
summary(model_a)$coefficients
summary(model_b)$coefficients

#c.------------------------------------------------------
# 使用 anova 比較模型，檢驗固定效應的顯著性
anova(model_a, model_b)

#d.------------------------------------------------------
# 載入隨機效應所需的套件
library(plm)

# 估計隨機效應模型
model_d <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, 
               data = star, 
               index = c("schid"), 
               model = "random")

# 顯示隨機效應模型結果
summary(model_d)

# 比較模型 a、b 和 d 的係數
cat("\nModel a (OLS) Coefficients:\n")
print(summary(model_a)$coefficients)
cat("\nModel b (Fixed Effects) Coefficients:\n")
print(summary(model_b)$coefficients)
cat("\nModel d (Random Effects) Coefficients:\n")
print(coef(model_d))

# 執行 Breusch-Pagan (LM) 檢驗以測試隨機效應
plmtest(model_d, type = "bp")

#e.------------------------------------------------------
# 提取係數和標準誤
fe_coefs <- summary(model_b)$coefficients[c("small", "aide", "tchexper", "white_asian", "freelunch", "boy"), ]
re_coefs <- coef(summary(model_d))[c("small", "aide", "tchexper", "white_asian", "freelunch", "boy"), ]

# 定義計算係數差異 t 檢驗的函數
t_test_fe_re <- function(fe_est, re_est, fe_se, re_se) {
  t_stat <- (fe_est - re_est) / sqrt(fe_se^2 + re_se^2)
  p_value <- 2 * pnorm(-abs(t_stat))
  return(list(t_stat = t_stat, p_value = p_value))
}

# 對每個變數執行 t 檢驗
variables <- c("small", "aide", "tchexper", "white_asian", "freelunch", "boy")
t_results <- lapply(variables, function(var) {
  fe_est <- fe_coefs[var, "Estimate"]
  re_est <- re_coefs[var, "Estimate"]
  fe_se <- fe_coefs[var, "Std. Error"]
  re_se <- re_coefs[var, "Std. Error"]
  t_test_fe_re(fe_est, re_est, fe_se, re_se)
})

# 顯示結果
cat("\nT-tests for differences between fixed and random effects coefficients:\n")
for (i in 1:length(variables)) {
  cat(sprintf("%s: t-statistic = %.3f, p-value = %.3f\n", 
              variables[i], t_results[[i]]$t_stat, t_results[[i]]$p_value))
}

#f.------------------------------------------------------
# 創建學校層面的變數平均值
library(dplyr)
star_means <- star %>%
  group_by(schid) %>%
  summarise(
    mean_small = mean(small, na.rm = TRUE),
    mean_aide = mean(aide, na.rm = TRUE),
    mean_tchexper = mean(tchexper, na.rm = TRUE),
    mean_white_asian = mean(white_asian, na.rm = TRUE),
    mean_freelunch = mean(freelunch, na.rm = TRUE)
  )

# 將學校平均值合併回原始數據
star <- star %>% left_join(star_means, by = "schid")

# 估計 Mundlak 模型（包含組平均值）
model_mundlak <- lm(readscore ~ small + aide + tchexper + white_asian + freelunch +
                      mean_small + mean_aide + mean_tchexper + mean_white_asian + mean_freelunch, 
                    data = star)

# 顯示 Mundlak 模型結果
summary(model_mundlak)

# 檢驗組平均值的聯合顯著性（Mundlak 檢驗）
library(car)
mundlak_test <- linearHypothesis(model_mundlak, 
                                 c("mean_small", "mean_aide", "mean_tchexper", 
                                   "mean_white_asian", "mean_freelunch"))
print(mundlak_test)