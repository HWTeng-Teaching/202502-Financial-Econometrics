
packages <- c("POE5Rdata", "AER", "car", "lmtest", "sandwich", "boot", "ggplot2")
installed <- packages %in% installed.packages()
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)

# 載入資料 -----------------------------------------------------------------------
data("mroz")
married_data <- subset(mroz, lfp == 1)

# 10.18 (a) 母親與父親是否大學畢業指標變數 -------------------------------------
married_data$mothercoll <- ifelse(married_data$mothereduc > 12, 1, 0)
married_data$fathercoll <- ifelse(married_data$fathereduc > 12, 1, 0)

pct_mother <- mean(married_data$mothercoll) * 100
pct_father <- mean(married_data$fathercoll) * 100

cat("母親有大學教育者比例：", round(pct_mother, 2), "%\n")
cat("父親有大學教育者比例：", round(pct_father, 2), "%\n")

# 10.18 (b) 計算相關係數矩陣 -----------------------------------------------------
corr_matrix <- cor(married_data[, c("educ", "mothercoll", "fathercoll")])
print(round(corr_matrix, 4))

# 10.18 (c) 第一個 IV 模型 --------------------------------------------------------
iv_model1 <- ivreg(log(wage) ~ educ + exper + I(exper^2) |
                     mothercoll + exper + I(exper^2),
                   data = married_data)
summary(iv_model1)
confint(iv_model1)

# 10.18 (d) 檢查 IV 模型診斷 -----------------------------------------------------
summary(iv_model1, diagnostics = TRUE)

# 10.18 (e)(f)(g) 使用雙工具變數 --------------------------------------------------
iv_model2 <- ivreg(log(wage) ~ educ + exper + I(exper^2) |
                     mothercoll + fathercoll + exper + I(exper^2),
                   data = married_data)
summary(iv_model2)
confint(iv_model2)
summary(iv_model2, diagnostics = TRUE)

# -------------------------------------------------------------------------------
# 10.20 CAPM 模型與工具變數分析 ---------------------------------------------------
data("capm5")
capm5$ex_mkt <- capm5$mkt - capm5$riskfree
capm5$ex_msft <- capm5$msft - capm5$riskfree

# (a) CAPM 模型回歸
capm <- lm(ex_msft ~ ex_mkt, data = capm5)
summary(capm)

# (b) 工具變數 rank 回歸 mkt
capm5$rank <- rank(capm5$ex_mkt)
first_stage <- lm(mkt ~ rank, data = capm5)
summary(first_stage)

# (c) Hausman 檢定
capm5$v_hat <- first_stage$residuals
hausman_model <- lm(ex_msft ~ ex_mkt + v_hat, data = capm5)
summary(hausman_model)

# (d) IV regression
capm_iv <- ivreg(ex_msft ~ ex_mkt | rank, data = capm5)
summary(capm_iv)

# (e) 增加 pos 作為額外工具變數
capm5$pos <- ifelse(capm5$ex_mkt > 0, 1, 0)
first_stage2 <- lm(ex_mkt ~ pos + rank, data = capm5)
summary(first_stage2)

# (f) Hausman 檢定第 2 次
capm5$v_hat2 <- first_stage2$residuals
hausman_model2 <- lm(ex_msft ~ ex_mkt + v_hat2, data = capm5)
summary(hausman_model2)

# (g) 第二個 IV 回歸
capm_iv2 <- ivreg(ex_msft ~ ex_mkt | pos + rank, data = capm5)
summary(capm_iv2)

# (h) Sargan 過度識別檢定
re <- resid(capm_iv2)
sargan <- lm(re ~ rank + pos, data = capm5)
S <- nrow(capm5) * summary(sargan)$r.squared
p_value <- 1 - pchisq(S, df = 1)
cat("Sargan 統計量 =", round(S, 4), "\n")
cat("p-value =", round(p_value, 4), "\n")

# -------------------------------------------------------------------------------
# 10.24 工具變數模型的異質性檢定與穩健性分析 --------------------------------------

# (a) 繪製殘差圖檢查異質變異
iv_model3 <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                     exper + I(exper^2) + mothereduc + fathereduc,
                   data = married_data)
married_data$e_iv <- resid(iv_model3)

ggplot(married_data, aes(x = exper, y = e_iv)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "purple") +
  labs(x = "EXPER", y = "IV residuals") +
  theme_minimal()

# (b) Breusch-Pagan 檢定
married_data$e_iv_sq <- married_data$e_iv^2
bp_model <- lm(e_iv_sq ~ exper, data = married_data)
NR2 <- nrow(married_data) * summary(bp_model)$r.squared
cat("NR² =", round(NR2, 4), "\n")
cat("p-value =", round(1 - pchisq(NR2, df = 1), 4), "\n")

# (c) 估計 robust 標準誤
robust_result <- coeftest(iv_model3, vcov = vcovHC(iv_model3, type = "HC1"))
print(robust_result)

b <- robust_result["educ", "Estimate"]
se <- robust_result["educ", "Std. Error"]
cat("95% confidence interval for EDUC:", round(b - 1.96 * se, 4), "to", round(b + 1.96 * se, 4), "\n")

# (d) Bootstrap 推論
boot_iv <- function(data, indices) {
  d <- data[indices, ]
  coef(ivreg(log(wage) ~ exper + I(exper^2) + educ |
               exper + I(exper^2) + mothereduc + fathereduc,
             data = d))["educ"]
}

set.seed(123)
boot_result <- boot(data = married_data, statistic = boot_iv, R = 200)
boot_ci <- boot.ci(boot_result, type = "norm")

cat("EDUC 係數 (bootstrap):", round(mean(boot_result$t), 4), "\n")
cat("Bootstrap SE:", round(sd(boot_result$t), 4), "\n")
cat("95% C.I. (normal method):", 
    round(boot_ci$normal[2], 4), "to", round(boot_ci$normal[3], 4), "\n")
