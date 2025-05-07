rm(list=ls()) 
library(POE5Rdata)
data(mroz)
# 篩選出參與勞動
#市場的 married women
#(a)
mroz2 <- subset(mroz, lfp == 1)

# 建立虛擬變數：父母是否受過大學教育（>12年）
mroz2$mothercoll <- ifelse(mroz2$mothereduc > 12, 1, 0)
mroz2$fathercoll <- ifelse(mroz2$fathereduc > 12, 1, 0)

# 計算比例
mother_pct <- mean(mroz2$mothercoll) * 100
father_pct <- mean(mroz2$fathercoll) * 100

cat(round(mother_pct, 2), "%\n")
cat(round(father_pct, 2), "%\n")



# b. 計算相關係數矩陣
cor_matrix <- cor(mroz2[, c("educ", "mothercoll", "fathercoll")], use = "complete.obs")
print("educ, mothereduc 與 fathereduc 的相關係數矩陣：")
print(cor_matrix)



##(c)
library(AER)

# 建立 exper 平方
mroz2$lnwage <- log(mroz2$wage)
mroz2$exper2 <- mroz2$exper^2

# 使用 MOTHERCOLL 作為 EDU 的工具變數
iv_model_c <- ivreg(lnwage ~ educ + exper + exper2 | mothercoll + exper + exper2, data = mroz2)

# 檢視估計結果
summary(iv_model_c)

# 取得 EDU 的 95% 信賴區間
confint(iv_model_c, level = 0.95)



##(d)
first_stage_d <- lm(educ ~ mothercoll + exper + exper2, data = mroz2)
summary(first_stage_d)

library(car)
linearHypothesis(first_stage_d, "mothercoll = 0")


##(e)
# 使用兩個 IV 估計 lwage 模型
iv_model_e <- ivreg(lnwage ~ educ + exper + exper2 | mothercoll + fathercoll + exper + exper2, data = mroz2)

# 回歸摘要
summary(iv_model_e)

# EDU 的 95% 信賴區間
confint(iv_model_e, level = 0.95)



##(f)
first_stage_f <- lm(educ ~ mothercoll + fathercoll + exper + exper2, data = mroz2)
summary(first_stage_f)

# 聯合檢定兩個 IV 是否顯著
library(car)
linearHypothesis(first_stage_f, c("mothercoll = 0", "fathercoll = 0"))


##(g)
# 取得 2SLS 的殘差
mroz2$u_iv_e <- resid(iv_model_e)

# 用工具變數解釋殘差：若顯著 ⇒ 工具無效
sargan_model <- lm(u_iv_e ~ mothercoll + fathercoll, data = mroz2)
summary(sargan_model)

# 聯合檢定工具是否與誤差有關
linearHypothesis(sargan_model, c("mothercoll = 0", "fathercoll = 0"))

# 計算 Sargan 統計量：n × R²
S <- nrow(mroz2) * summary(sargan_model)$r.squared

# 計算 p-value（自由度 = 超額工具個數 = 2 - 1 = 1）
(p_value <- 1 - pchisq(S, df = 1))




##Q20
rm(list=ls())  
library(POE5Rdata)  
data("capm5") 

##(a)

# 建立風險溢酬
capm5$riskfree_msft <- capm5$msft - capm5$riskfree
capm5$riskfree_mkt <- capm5$mkt - capm5$riskfree

# 執行 OLS 回歸
model_capm <- lm(riskfree_msft ~ riskfree_mkt, data = capm5)
summary(model_capm)


##(b)
# 建立 RANK（對 market risk premium 排序）
capm5$RANK <- rank(capm5$riskfree_mkt)


# 第一階段回歸：market risk premium ~ RANK
stage1_model <- lm(riskfree_mkt ~ RANK, data = capm5)
summary(stage1_model)



##(c)
# 第一階段回歸：用 RANK 解釋 market risk premium
stage1_model <- lm(riskfree_mkt ~ RANK, data = capm5)
capm5$what <- resid(stage1_model)  # 將殘差存在 what 欄位


# 拿到第一階段殘差
capm5$vhat <- resid(first_stage)

# 擴增模型：風險溢酬 ~ market risk premium + 殘差項 vhat
augmented_model <- lm(riskfree_msft ~ riskfree_mkt + what, data = capm5)
summary(augmented_model)




##(d)
install.packages("AER")
library(AER)
iv_model_d <- ivreg(riskfree_msft ~ riskfree_mkt | RANK, data = capm5)
summary(iv_model_d)



##(e)
# 建立 POS 變數
capm5$POS <- as.integer((capm5$riskfree_mkt) > 0)

# 第一階段回歸：用 RANK 和 POS 預測 riskfree_mkt
first_stage <- lm(riskfree_mkt ~ RANK + POS, data = capm5)
summary(first_stage)

library(car)
linearHypothesis(first_stage, c("RANK = 0", "POS = 0"))




##(f)
# 加入第一階段殘差項（從 e 小題的第一階段模型）
first_stage_f <- lm(riskfree_mkt ~ RANK + POS, data = capm5)
capm5$u_iv <- resid(first_stage_f)  # u_iv 是第一階段的殘差

# 將 u_iv 加入到原始 OLS 模型（CAPM）中
hausman_model <- lm(riskfree_msft ~ riskfree_mkt + u_iv, data = capm5)

# 查看 u_iv 的係數是否顯著
summary(hausman_model)



##(g)
library(AER)

# 估計 IV/2SLS 模型，使用 RANK 和 POS 作為工具變數
iv_model_g <- ivreg(riskfree_msft ~ riskfree_mkt | RANK + POS, data = capm5)

# 查看估計結果
summary(iv_model_g)



##(h)
# 取得 IV 模型殘差（g 題模型）
iv_resid_h <- resid(iv_model_g)

# 使用這些殘差對所有工具變數回歸（不包含內生變數）
sargan_model <- lm(iv_resid_h ~ RANK + POS, data = capm5)

# 計算 Sargan 檢定統計量
sargan_R2 <- summary(sargan_model)$r.squared
N <- nrow(capm5)
sargan_stat <- N * sargan_R2

# 計算 p 值（自由度 = 工具變數個數 - 內生變數個數 = 2 - 1 = 1）
pval <- 1 - pchisq(sargan_stat, df = 1)

# 輸出結果
cat("Sargan 統計量 =", round(sargan_stat, 3), ", p 值 =", round(pval, 4), "\n")




##Q24
#(a)
# 套件
library(AER)      # for ivreg()
library(ggplot2)  # for plotting
library(dplyr)

# 篩選有參與勞動的樣本
data("mroz")
mroz2 <- subset(mroz, wage > 0)
mroz2 <- mroz %>% filter(lfp == 1)

# 執行 IV/2SLS 回歸：使用 mothereduc 和 fathereduc 為工具變數
iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) |
                    hmothereduc + hfathereduc + exper + I(exper^2),
                  data = mroz2)

# 取得殘差
iv_resid <- resid(iv_model)

# 加入 residuals 到原始資料中
mroz2$iv_resid <- iv_resid

# 繪圖：residuals vs exper
ggplot(mroz2, aes(x = exper, y = iv_resid)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "IV residuals vs Experience (EXPER)",
       x = "EXPER",
       y = "IV residuals") +
  theme_minimal()


##(b)
mroz2$iv_resid_sq <- mroz2$iv_resid^2

# 用 exper 解釋殘差平方
bp_model <- lm(iv_resid_sq ~ exper, data = mroz2)

# 計算 nR² 統計量
nR2 <- summary(bp_model)$r.squared * nrow(mroz2)
pval <- 1 - pchisq(nR2, df = 1)

# 顯示結果
cat("nR² =", round(nR2, 4), ", p-value =", round(pval, 6), "\n")




##(C)
# robust SE with sandwich estimator
library(sandwich)
library(lmtest)
iv_model <- ivreg(log(wage) ~ educ + exper + I(exper^2) | mothereduc + fathereduc + exper + I(exper^2), data = mroz_clean)

# Robust standard errors
robust_se <- sqrt(diag(vcovHC(iv_model, type = "HC1")))

# 顯示結果與 robust t 值
coeftest(iv_model, vcov = vcovHC(iv_model, type = "HC1"))

# 教育年數的 95% CI，用 robust SE 計算
coef_educ <- coef(iv_model)["educ"]
se_educ <- robust_se["educ"]

lower <- coef_educ - 1.96 * se_educ
upper <- coef_educ + 1.96 * se_educ
cat("Robust 95% CI for EDUC: [", round(lower, 4), ",", round(upper, 4), "]\n")


##(d)
# 需要 boot 套件
library(boot)

# 設定自定義 bootstrap 函數：只回傳教育係數
boot_iv_func <- function(data, indices) {
  d <- data[indices, ]
  model <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                   exper + I(exper^2) + mothereduc + fathereduc, data = d)
  return(coef(model)["educ"])
}

# 設定 seed 以利重現
set.seed(123)
boot_iv <- boot(data = mroz2, statistic = boot_iv_func, R = 200)

# Bootstrap 標準誤
boot_se <- sd(boot_iv$t)

# Bootstrap CI
boot_ci <- coef(iv_model)["educ"] + c(-1, 1) * 1.96 * boot_se
print(boot_ci)
