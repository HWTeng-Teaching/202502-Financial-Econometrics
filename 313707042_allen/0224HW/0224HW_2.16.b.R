# 載入必要套件
library(POE5Rdata)

# 載入 capm5 數據
data("capm5", package = "POE5Rdata")

# 計算市場風險溢酬與個股超額報酬
capm5$mkt_rf <- capm5$mkt - capm5$riskfree
stock_cols <- c("ge", "ibm", "ford", "msft", "dis", "xom")

for (stock in stock_cols) {
  capm5[[paste0(stock, "_rf")]] <- capm5[[stock]] - capm5$riskfree
}

# 定義 CAPM 迴歸函數
capm_regression <- function(stock_rf, market_rf) {
  lm(stock_rf ~ market_rf)
}

# 執行 CAPM 迴歸並提取 Beta 值
beta_values <- data.frame(
  Company = stock_cols,
  Beta = sapply(stock_cols, function(stock) coef(capm_regression(capm5[[paste0(stock, "_rf")]], capm5$mkt_rf))[2])
)

# 顯示 Beta 值
print(beta_values)

# 找出最具攻擊性與防禦性的公司
most_aggressive <- beta_values[which.max(beta_values$Beta), ]
most_defensive <- beta_values[which.min(beta_values$Beta), ]

print(paste("🚀 最具攻擊性公司:", most_aggressive$Company, "Beta =", most_aggressive$Beta))
print(paste("🛡️ 最具防禦性公司:", most_defensive$Company, "Beta =", most_defensive$Beta))
