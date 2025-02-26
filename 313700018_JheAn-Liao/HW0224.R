# 🌟 清空環境
rm(list = ls())

# 🌟 載入必要套件
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# 🔗 下載並載入 capm5 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(capm5)

# ✅ 計算市場與各公司超額報酬率
capm5 <- capm5 %>%
  mutate(
    MKT_excess = mkt - riskfree,        
    GE_excess = ge - riskfree,          
    IBM_excess = ibm - riskfree,
    Ford_excess = ford - riskfree,
    Microsoft_excess = msft - riskfree,
    Disney_excess = dis - riskfree,
    Exom_excess = xom - riskfree
  )

# 📊 定義 CAPM 模型回歸函數 (計算有截距與無截距 Beta 並求差異)
estimate_betas <- function(firm_excess) {
  # 有截距模型
  model_with_intercept <- lm(firm_excess ~ MKT_excess, data = capm5)
  beta_with_intercept <- coef(model_with_intercept)["MKT_excess"]
  
  # 無截距模型
  model_no_intercept <- lm(firm_excess ~ 0 + MKT_excess, data = capm5)
  beta_no_intercept <- coef(model_no_intercept)["MKT_excess"]
  
  # 差異計算
  beta_diff <- beta_with_intercept - beta_no_intercept
  
  return(list(
    Beta_Intercept = beta_with_intercept,
    Beta_No_Intercept = beta_no_intercept,
    Beta_Difference = beta_diff
  ))
}

# 🏢 計算六間公司的 Beta 值
companies <- c("GE_excess", "IBM_excess", "Ford_excess",
               "Microsoft_excess", "Disney_excess", "Exom_excess")

results <- lapply(companies, function(x) estimate_betas(capm5[[x]]))
names(results) <- c("GE", "IBM", "Ford", "Microsoft", "Disney", "Exom")

# 📊 擬合 Microsoft 股票的 CAPM 模型
msft_model <- lm(Microsoft_excess ~ MKT_excess, data = capm5)
summary(msft_model)

# 提取 Alpha (截距) 與 Beta (斜率)
alpha_msft <- round(coef(msft_model)["(Intercept)"], 4)
beta_msft <- round(coef(msft_model)["MKT_excess"], 4)
# 🎨 繪製散點圖與回歸線，並優化標註位置
ggplot(capm5, aes(x = MKT_excess, y = Microsoft_excess)) +
  geom_point(color = "blue", alpha = 0.6) +                     # 散點圖
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +  # 回歸線
  annotate("text", x = -0.2, y = 0.3,
           label = paste0("Intercept (Alpha): ", alpha_msft),
           hjust = 0, vjust = 1, color = "darkgreen", size = 4, fontface = "bold") +
  annotate("text", x = -0.2, y = 0.25,
           label = paste0("Slope (Beta): ", beta_msft),
           hjust = 0, vjust = 1, color = "purple", size = 4, fontface = "bold") +
  labs(
    title = "Microsoft regression line",
    subtitle = paste0("MSFT Excess = ", alpha_msft, " + ", beta_msft, " × MKT Excess"),
    x = "Market Excess Return (MKT - RISKFREE)",
    y = "Microsoft Excess Return (MSFT - RISKFREE)"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 12),
    legend.position = "none"
  )


# 📝 整理並呈現結果 (以表格形式)
beta_comparison <- tibble(
  Company = names(results),
  Beta_Intercept = sapply(results, function(x) round(x$Beta_Intercept, 6)),
  Beta_No_Intercept = sapply(results, function(x) round(x$Beta_No_Intercept, 6)),
  Beta_Difference = sapply(results, function(x) round(x$Beta_Difference, 6))
) %>%
  arrange(desc(Beta_Intercept))  # 依有截距 Beta 值由高到低排序

# 📈 顯示最終結果
print(beta_comparison)


