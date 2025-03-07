#a
install.packages("remotes")  # 確保 remotes 套件已安裝
remotes::install_github("ccolonescu/POE5Rdata")  # 需確認 GitHub Repo 位置
library(POE5Rdata)
data(capm5)

# 載入必要套件
install.packages("sandwich")  # 若未安裝
install.packages("lmtest")    # 若未安裝
install.packages("dplyr")     # 安裝 dplyr
install.packages("tidyverse")  # 若未安裝，先安裝 tidyverse
install.packages("knitr")  # 安裝 knitr
library(knitr)  # 載入 knitr
library(tidyr)
library(sandwich)
library(lmtest)
library(dplyr)
library(purrr)

# 查看數據內容
ls()  # 列出環境中的變數
head(capm5)  # 查看 capm5 數據前幾行
str(capm5)   # 檢查數據結構
summary(capm5) # 顯示數據摘要

# 計算風險溢酬
capm5$excess_ret <- capm5$ret - capm5$riskfree  # 個股風險溢酬
capm5$mkt_excess <- capm5$mkt - capm5$riskfree  # 市場風險溢酬

# 確認計算結果
head(capm5)

------------

# 檢查 capm5 的變數名稱
names(capm5)

# 查看前幾行數據
head(capm5)

# 查看數據結構
str(capm5)



# 確保使用 tidyr 來轉換為長格式
library(tidyr)
library(dplyr)

# 轉換成長格式
capm_long <- capm5 %>%
  pivot_longer(cols = c(ge, ibm, ford, msft, dis, xom), 
               names_to = "firm", 
               values_to = "ret")

# 計算風險溢酬
capm_long <- capm_long %>%
  mutate(excess_ret = ret - riskfree,   # 計算個股風險溢酬
         mkt_excess = mkt - riskfree)   # 計算市場風險溢酬

# 確認轉換是否正確
head(capm_long)


#二、確保回歸結果是獨立的

# 確保 dplyr 已載入
library(dplyr)

# 使用 nest() + map() 來確保每家公司獨立回歸
library(purrr)

# 針對每家公司的數據進行回歸
capm_results <- capm_long %>%
  group_by(firm) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(excess_ret ~ mkt_excess, data = .x)),
         summary = map(model, summary),
         alpha = map_dbl(summary, ~ .x$coefficients[1, 1]),    # 截距 α_j
         beta = map_dbl(summary, ~ .x$coefficients[2, 1]),     # 斜率 β_j
         alpha_se = map_dbl(summary, ~ .x$coefficients[1, 2]), # α_j 的標準誤
         beta_se = map_dbl(summary, ~ .x$coefficients[2, 2])   # β_j 的標準誤
  ) %>%
  select(firm, alpha, alpha_se, beta, beta_se)

# 顯示結果
print(capm_results)

#三、表格化輸出

# 載入 knitr 來產生美觀表格
library(knitr)

# 產生對應的表格
capm_table_formatted <- capm_results %>%
  mutate(
    alpha_formatted = sprintf("%.6f", alpha),
    alpha_se_formatted = sprintf("(%.5f)", alpha_se),
    beta_formatted = sprintf("%.3f", beta),
    beta_se_formatted = sprintf("(%.4f)", beta_se)
  ) %>%
  select(firm, alpha_formatted, alpha_se_formatted, beta_formatted, beta_se_formatted)

# 使用 kable 輸出表格
kable(capm_table_formatted, col.names = c("Firm", "b1 = α_j", "", "b2 = β_j", ""))


# 載入 Rdata 檔案
load("capm5.Rdata")

# 載入必要套件
install.packages("sandwich")  # 若未安裝
install.packages("lmtest")    # 若未安裝
install.packages("dplyr")     # 安裝 dplyr
install.packages("tidyverse")  # 若未安裝，先安裝 tidyverse
install.packages("knitr")  # 安裝 knitr
library(knitr)  # 載入 knitr
library(tidyr)
library(sandwich)
library(lmtest)
library(dplyr)
library(purrr)

# 查看數據內容
ls()  # 列出環境中的變數
head(capm5)  # 查看 capm5 數據前幾行
str(capm5)   # 檢查數據結構
summary(capm5) # 顯示數據摘要

# 計算風險溢酬
capm5$excess_ret <- capm5$ret - capm5$riskfree  # 個股風險溢酬
capm5$mkt_excess <- capm5$mkt - capm5$riskfree  # 市場風險溢酬

# 確認計算結果
head(capm5)

------------

# 檢查 capm5 的變數名稱
names(capm5)

# 查看前幾行數據
head(capm5)

# 查看數據結構
str(capm5)

#繪製 Microsoft (MSFT) 的 CAPM 回歸圖

# 載入 ggplot2
library(ggplot2)

# 選取 Microsoft (MSFT) 數據
msft_data <- capm_long %>% filter(firm == "msft")

# 建立回歸模型
msft_model <- lm(excess_ret ~ mkt_excess, data = msft_data)

# 繪製散佈圖 + 迴歸線
ggplot(msft_data, aes(x = mkt_excess, y = excess_ret)) +
  geom_point(color = "blue", alpha = 0.6) +  # 資料點
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # 迴歸線
  labs(title = "CAPM 回歸 (Microsoft)", 
       x = "市場風險溢酬", 
       y = "微軟風險溢酬") +
  theme_minimal()



------

d.

# 針對每家公司進行無截距回歸 (α_j = 0)
capm_no_intercept <- capm_long %>%
  group_by(firm) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(excess_ret ~ mkt_excess + 0, data = .x)),  # 無截距回歸
    summary = map(model, summary),
    beta_no_intercept = map_dbl(summary, ~ .x$coefficients["mkt_excess", "Estimate"]),  # 估計 β_j
    beta_se_no_intercept = map_dbl(summary, ~ .x$coefficients["mkt_excess", "Std. Error"])  # β_j 的標準誤
  ) %>%
  select(firm, beta_no_intercept, beta_se_no_intercept)

# 顯示結果
print(capm_no_intercept)


# 合併原始 β_j (有截距) 與新的 β_j (無截距)
capm_beta_comparison <- capm_results_robust %>%
  select(firm, beta, beta_pval) %>%
  left_join(capm_no_intercept, by = "firm") %>%
  mutate(beta_change = beta_no_intercept - beta)  # 計算 β 變化量

# 顯示比較結果
print(capm_beta_comparison)
