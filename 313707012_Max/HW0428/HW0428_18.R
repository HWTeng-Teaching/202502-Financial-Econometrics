library(POE5Rdata)
data("mroz")

#a.------------------------------------------------------
# 保留只參與勞動市場的觀察值（與前面一樣）
mroz_working <- subset(mroz, lfp == 1)  # 428 筆觀察值

# 創造新變數
mroz_working$MOTHERCOLL <- ifelse(mroz_working$mothereduc > 12, 1, 0)
mroz_working$FATHERCOLL <- ifelse(mroz_working$fathereduc > 12, 1, 0)

# 計算百分比
mother_coll_pct <- mean(mroz_working$MOTHERCOLL) * 100
father_coll_pct <- mean(mroz_working$FATHERCOLL) * 100

# 顯示結果
cat("母親有部分大學教育的比例：", round(mother_coll_pct, 2), "%\n")
cat("父親有部分大學教育的比例：", round(father_coll_pct, 2), "%\n")

#b.------------------------------------------------------
# 計算 EDUC, MOTHERCOLL, FATHERCOLL 之間的相關係數
correlations <- cor(mroz_working[, c("educ", "MOTHERCOLL", "FATHERCOLL")])
print(correlations)


#c.------------------------------------------------------
# 載入必要套件
library(AER)
mroz_working$lwage <- log(mroz_working$wage)
mroz_working$exper2 <- mroz_working$exper^2
iv_model <- ivreg(lwage ~ exper + exper2 + educ | exper + exper2 + MOTHERCOLL, data = mroz_working)
summary(iv_model,diagnostics = TRUE)
confint(iv_model, level = 0.95)["educ", ]
cat('95% Confidence Interval: [',confint(iv_model, level = 0.95)["educ", ],']')

#d.------------------------------------------------------
# 第一階段回歸：educ 對 MOTHERCOLL 與控制變數
first_stage_model <- lm(educ ~ MOTHERCOLL + exper + exper2, data = mroz_working)
summary(first_stage_model)
# 若要針對 MOTHERCOLL 單獨做 F 檢定，可使用 car 套件
library(car)
linearHypothesis(first_stage_model, "MOTHERCOLL = 0")


#e.------------------------------------------------------
# 二階段最小平方法（2SLS）：使用兩個工具變數
iv_model2 <- ivreg(lwage ~ exper + exper2 + educ | exper + exper2 + MOTHERCOLL + FATHERCOLL, data = mroz_working)
summary(iv_model2)

# 計算 95% 信賴區間（EDUC）
ci_educ <- confint(iv_model2, level = 0.95)["educ", ]
cat("95% Confidence Interval for EDUC (with 2 IVs): [", ci_educ[1], ",", ci_educ[2], "]\n")

#f.------------------------------------------------------
# 第一階段回歸
first_stage2 <- lm(educ ~ exper + exper2 + MOTHERCOLL + FATHERCOLL, data = mroz_working)
summary(first_stage2)
# 使用 car 套件做聯合顯著性檢定（H0: MOTHERCOLL = FATHERCOLL = 0）
linearHypothesis(first_stage2, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

#g.------------------------------------------------------
# 計算 IV 模型的殘差
resid_iv <- resid(iv_model2)

# 建立 Sargan 檢定：以殘差為應變數，用所有 IV 回歸
sargan_test <- lm(resid_iv ~ exper + exper2 + MOTHERCOLL + FATHERCOLL, data = mroz_working)
summary(sargan_test)

# 取 R² 並計算檢定統計量（樣本數 * R²）
n <- nrow(mroz_working)
sargan_stat <- n * summary(sargan_test)$r.squared

# 計算 p 值（自由度 = 工具變數數量 - 內生變數數量 = 2 - 1 = 1）
p_value <- 1 - pchisq(sargan_stat, df = 1)

cat("Sargan statistic:", sargan_stat, "\n")
cat("p-value:", p_value, "\n")
