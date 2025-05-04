# 載入所需的套件和數據集
library(POE5Rdata)
data("capm5")

# 計算 Microsoft 的風險溢價 (r_j - r_f)
capm5$msft_excess <- capm5$msft - capm5$riskfree

# 計算市場溢價 (r_m - r_f)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree

# 使用 OLS 估計 CAPM 模型：msft_excess ~ mkt_excess
model <- lm(msft_excess ~ mkt_excess, data = capm5)

# 輸出回歸結果
summary_model <- summary(model)
cat("CAPM 模型 OLS 估計結果：\n")
print(summary_model)

# 提取 beta 值
beta <- coef(model)["mkt_excess"]
cat("Microsoft 股票的 beta 值：", round(beta, 3), "\n")

# 判斷風險類別
if (beta > 1) {
  cat("Microsoft 股票相對於市場組合風險較高。\n")
} else if (beta < 1) {
  cat("Microsoft 股票相對於市場組合風險較低（相對安全）。\n")
} else {
  cat("Microsoft 股票相對於市場組合風險相當。\n")
}

# b.-------------------
# 計算市場溢價 (r_m - r_f)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree

# 檢查市場溢價是否按升序排列
cat("市場溢價 (mkt_excess) 的前幾個值：\n")
print(head(capm5$mkt_excess, 10))  # 查看前10個值，檢查排序
cat("市場溢價是否已排序：", order(capm5$mkt_excess), "\n")
# 注意：R 中沒有內建 is.sorted()，可以用 order() 檢查
if (identical(order(capm5$mkt_excess), 1:nrow(capm5))) {
  cat("市場溢價已按升序排列。\n")
} else {
  cat("市場溢價未按升序排列。\n")
}

# 創建排名變量 RANK
capm5$RANK <- rank(capm5$mkt_excess)

# 第一階段回歸：mkt_excess ~ RANK
first_stage <- lm(mkt_excess ~ RANK, data = capm5)

# 輸出第一階段回歸結果
summary_first_stage <- summary(first_stage)
cat("第一階段回歸結果：\n")
print(summary_first_stage)

# 提取 R^2 值
r_squared <- summary_first_stage$r.squared
cat("第一階段回歸的 R^2 值：", round(r_squared, 3), "\n")

# 判斷工具變量強度（基於 R^2，輔以 F 統計量）
f_statistic <- summary_first_stage$fstatistic[1]  # 提取 F 統計量
cat("F 統計量：", round(f_statistic, 3), "\n")
if (f_statistic > 10) {
  cat("F 統計量 > 10，RANK 可能是強工具變量。\n")
} else {
  cat("F 統計量 <= 10，RANK 可能是弱工具變量。\n")
}

# c.-----------------------
# 計算 Microsoft 的風險溢價 (r_j - r_f)
capm5$msft_excess <- capm5$msft - capm5$riskfree

# 計算市場溢價 (r_m - r_f)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree

# 創建排名變量 RANK
capm5$RANK <- rank(capm5$mkt_excess)

# 第一階段回歸：mkt_excess ~ RANK
first_stage <- lm(mkt_excess ~ RANK, data = capm5)

# 提取第一階段殘差 v_hat
capm5$v_hat <- residuals(first_stage)

# 增廣模型：msft_excess ~ mkt_excess + v_hat
augmented_model <- lm(msft_excess ~ mkt_excess + v_hat, data = capm5)

# 輸出增廣模型結果
summary_augmented <- summary(augmented_model)
cat("增廣模型 OLS 估計結果：\n")
print(summary_augmented)

# 檢驗 v_hat 的係數顯著性
v_hat_pvalue <- coef(summary_augmented)["v_hat", "Pr(>|t|)"]
cat("v_hat 係數的 p 值：", round(v_hat_pvalue, 4), "\n")

# 在 1% 顯著性水平下檢驗
if (v_hat_pvalue < 0.01) {
  cat("在 1% 顯著性水平下，v_hat 顯著，市場回報 (mkt_excess) 內生。\n")
} else {
  cat("在 1% 顯著性水平下，v_hat 不顯著，無法拒絕市場回報 (mkt_excess) 外生的假設。\n")
}

# d.---------------------
# 載入所需的套件和數據集
library(AER)  # 用於 IV 回歸

# 計算 Microsoft 的風險溢價 (r_j - r_f)
capm5$msft_excess <- capm5$msft - capm5$riskfree

# 計算市場溢價 (r_m - r_f)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree

# 創建排名變量 RANK
capm5$RANK <- rank(capm5$mkt_excess)

# (a) 小題的 OLS 估計
ols_model <- lm(msft_excess ~ mkt_excess, data = capm5)
ols_beta <- coef(ols_model)["mkt_excess"]
cat("OLS 估計的 beta 值：", round(ols_beta, 3), "\n")

# IV/2SLS 估計，使用 RANK 作為工具變量
iv_model <- ivreg(msft_excess ~ mkt_excess | RANK, data = capm5)

# 輸出 IV 回歸結果
summary_iv <- summary(iv_model)
cat("IV/2SLS 估計結果：\n")
print(summary_iv)

# 提取 IV 估計的 beta 值
iv_beta <- coef(iv_model)["mkt_excess"]
cat("IV 估計的 beta 值：", round(iv_beta, 3), "\n")

# 比較 OLS 和 IV 估計
cat("OLS beta 與 IV beta 比較：\n")
if (iv_beta > ols_beta) {
  cat("IV beta > OLS beta，可能表明 OLS 因內生性向下偏誤。\n")
} else if (iv_beta < ols_beta) {
  cat("IV beta < OLS beta，可能表明其他因素影響估計。\n")
} else {
  cat("IV beta ≈ OLS beta，內生性影響可能不大。\n")
}

# 判斷 IV 估計是否符合期望
cat("期望判斷：由於 r_m - r_f 可能內生，IV 估計應修正 OLS 偏誤。如果 IV beta 顯著大於 OLS beta，符合內生性假設。\n")

# e.------------------------
# 計算市場溢價 (r_m - r_f)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree

# 創建排名變量 RANK
capm5$RANK <- rank(capm5$mkt_excess)

# 創建 POS 變量：若 mkt_excess > 0 則為 1，否則為 0
capm5$POS <- ifelse(capm5$mkt_excess > 0, 1, 0)

# 第一階段回歸：mkt_excess ~ RANK + POS
first_stage <- lm(mkt_excess ~ RANK + POS, data = capm5)

# 輸出第一階段回歸結果
summary_first_stage <- summary(first_stage)
cat("第一階段回歸結果：\n")
print(summary_first_stage)

# 提取 R^2 值
r_squared <- summary_first_stage$r.squared
cat("第一階段回歸的 R^2 值：", round(r_squared, 3), "\n")

# 提取 F 統計量
f_statistic <- summary_first_stage$fstatistic[1]
cat("F 統計量：", round(f_statistic, 3), "\n")

# 判斷工具變量強度
if (f_statistic > 10) {
  cat("F 統計量 > 10，RANK 和 POS 可能是強工具變量。\n")
} else {
  cat("F 統計量 <= 10，RANK 和 POS 可能是弱工具變量。\n")
}

# f.-------------------------------
# 計算 Microsoft 的風險溢價 (r_j - r_f)
capm5$msft_excess <- capm5$msft - capm5$riskfree

# 計算市場溢價 (r_m - r_f)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree

# 創建排名變量 RANK 和 POS
capm5$RANK <- rank(capm5$mkt_excess)
capm5$POS <- ifelse(capm5$mkt_excess > 0, 1, 0)

# 第一階段回歸：mkt_excess ~ RANK + POS
first_stage <- lm(mkt_excess ~ RANK + POS, data = capm5)

# 提取第一階段殘差 v_hat
capm5$v_hat <- residuals(first_stage)

# 增廣模型：msft_excess ~ mkt_excess + v_hat
augmented_model <- lm(msft_excess ~ mkt_excess + v_hat, data = capm5)

# 輸出增廣模型結果
summary_augmented <- summary(augmented_model)
cat("增廣模型 OLS 估計結果（Hausman 檢驗）：\n")
print(summary_augmented)

# 檢驗 v_hat 的係數顯著性
v_hat_pvalue <- coef(summary_augmented)["v_hat", "Pr(>|t|)"]
cat("v_hat 係數的 p 值：", round(v_hat_pvalue, 4), "\n")

# 在 1% 顯著性水平下檢驗
if (v_hat_pvalue < 0.01) {
  cat("在 1% 顯著性水平下，v_hat 顯著，市場回報 (mkt_excess) 內生。\n")
} else {
  cat("在 1% 顯著性水平下，v_hat 不顯著，無法拒絕市場回報 (mkt_excess) 外生的假設。\n")
}

# g.----------------------
# 計算 Microsoft 的風險溢價 (r_j - r_f)
capm5$msft_excess <- capm5$msft - capm5$riskfree

# 計算市場溢價 (r_m - r_f)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree

# 創建排名變量 RANK 和 POS
capm5$RANK <- rank(capm5$mkt_excess)
capm5$POS <- ifelse(capm5$mkt_excess > 0, 1, 0)

# (a) 小題的 OLS 估計
ols_model <- lm(msft_excess ~ mkt_excess, data = capm5)
ols_beta <- coef(ols_model)["mkt_excess"]
cat("OLS 估計的 beta 值：", round(ols_beta, 3), "\n")

# IV/2SLS 估計，使用 RANK 和 POS 作為工具變量
iv_model <- ivreg(msft_excess ~ mkt_excess | RANK + POS, data = capm5)

# 輸出 IV 回歸結果
summary_iv <- summary(iv_model)
cat("IV/2SLS 估計結果：\n")
print(summary_iv)

# 提取 IV 估計的 beta 值
iv_beta <- coef(iv_model)["mkt_excess"]
cat("IV 估計的 beta 值：", round(iv_beta, 3), "\n")

# 比較 OLS 和 IV 估計
cat("OLS beta 與 IV beta 比較：\n")
if (iv_beta > ols_beta) {
  cat("IV beta > OLS beta，可能表明 OLS 因內生性向下偏誤。\n")
} else if (iv_beta < ols_beta) {
  cat("IV beta < OLS beta，可能表明其他因素影響估計。\n")
} else {
  cat("IV beta ≈ OLS beta，內生性影響可能不大。\n")
}

# 判斷 IV 估計是否符合期望
cat("期望判斷：由於 r_m - r_f 可能內生（測量誤差），IV 估計應修正 OLS 偏誤。如果 IV beta 顯著大於 OLS beta，符合內生性假設。\n")

# h.--------------------
# 計算 Microsoft 的風險溢價 (r_j - r_f)
capm5$msft_excess <- capm5$msft - capm5$riskfree

# 計算市場溢價 (r_m - r_f)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree

# 創建排名變量 RANK 和 POS
capm5$RANK <- rank(capm5$mkt_excess)
capm5$POS <- ifelse(capm5$mkt_excess > 0, 1, 0)

# IV/2SLS 估計，使用 RANK 和 POS 作為工具變量
iv_model <- ivreg(msft_excess ~ mkt_excess | RANK + POS, data = capm5)

# 手動計算 IV 殘差
iv_fitted <- fitted(iv_model)  # 獲取擬合值
iv_residuals <- capm5$msft_excess - iv_fitted  # 計算殘差

# 將殘差加入數據框
capm5$iv_resid <- iv_residuals

# Sargan 檢驗：將殘差回歸到工具變量 RANK 和 POS
sargan_reg <- lm(iv_resid ~ RANK + POS, data = capm5)

# 計算 Sargan 統計量
summary_sargan <- summary(sargan_reg)
sargan_r_squared <- summary_sargan$r.squared
n <- nrow(capm5)  # 樣本量
sargan_statistic <- n * sargan_r_squared  # Sargan 統計量
df <- 1  # 自由度 = 工具變量數 - 內生變量數 = 2 - 1

# 計算 p 值 (使用 chi-squared 分佈)
p_value <- 1 - pchisq(sargan_statistic, df)

# 輸出結果
cat("Sargan 檢驗結果：\n")
cat("Sargan 統計量：", round(sargan_statistic, 3), "\n")
cat("p 值：", round(p_value, 4), "\n")

# 在 5% 顯著性水平下檢驗
if (p_value < 0.05) {
  cat("在 5% 顯著性水平下，p 值 < 0.05，拒絕工具變量外生的假設，RANK 和 POS 可能無效。\n")
} else {
  cat("在 5% 顯著性水平下，p 值 >= 0.05，無法拒絕工具變量外生的假設，RANK 和 POS 可能有效。\n")
}

