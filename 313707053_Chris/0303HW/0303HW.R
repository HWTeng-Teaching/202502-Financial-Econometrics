# 安裝並載入必要套件
install.packages("readxl")
install.packages("ggplot2")
library(readxl)
library(ggplot2)

# 讀取 Excel 檔案
df <- read_excel("capm5.xlsx")

# 確保 df 內容正確
print(head(df))

# 計算市場超額報酬 (r_m - r_f)
excess_market_return <- df$mkt - df$riskfree

# 計算個股超額報酬 (r_j - r_f)，確保欄位名稱正確
stock_columns <- c("ge", "ibm", "ford", "msft", "dis", "xom")  # 確保這些名稱與 df 內的欄位名稱一致
excess_returns <- df[, stock_columns] - df$riskfree

# 確保 excess_returns 建立成功
print(head(excess_returns))

# 迴歸分析
results <- data.frame(Firm = stock_columns,
                      Alpha = numeric(length(stock_columns)),
                      Beta = numeric(length(stock_columns)),
                      R_Squared = numeric(length(stock_columns)),
                      P_Value_Alpha = numeric(length(stock_columns)),
                      P_Value_Beta = numeric(length(stock_columns)))

# 逐一執行 CAPM 迴歸
for (i in 1:length(stock_columns)) {
  model <- lm(excess_returns[[i]] ~ excess_market_return)
  summary_model <- summary(model)
  
  results$Alpha[i] <- coef(model)[1]
  results$Beta[i] <- coef(model)[2]
  results$R_Squared[i] <- summary_model$r.squared
  results$P_Value_Alpha[i] <- summary_model$coefficients[1, 4]
  results$P_Value_Beta[i] <- summary_model$coefficients[2, 4]
}

# 顯示回歸結果
print(results)

# 檢驗 Alpha 是否顯著
significant_alpha <- results$P_Value_Alpha < 0.05
print(significant_alpha)

# 繪製 Microsoft (MSFT) 的回歸圖
msft_data <- data.frame(Excess_Market = excess_market_return, 
                        Excess_MSFT = excess_returns$msft)

ggplot(msft_data, aes(x = Excess_Market, y = Excess_MSFT)) +
  geom_point(color = "blue", alpha = 0.5) +   
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "CAPM Regression: Microsoft Stock",
       x = "Market Excess Return",
       y = "Microsoft Excess Return") +
  theme_minimal()

# 在 α = 0 條件下重新估計 Beta
beta_no_alpha <- sapply(stock_columns, function(stock) {
  coef(lm(excess_returns[[stock]] ~ excess_market_return - 1))[1]
})

# 比較 Beta 變化
beta_comparison <- data.frame(Firm = stock_columns,
                              Beta_With_Alpha = results$Beta,
                              Beta_No_Alpha = beta_no_alpha)

print(beta_comparison)
