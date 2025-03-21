# 計算斜率、截距
# 安裝並載入必要的套件
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

# 載入 CAPM 數據
library(POE5Rdata)
data("capm5")

print(capm5)
# 定義要分析的公司
company <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# 進行 CAPM 回歸分析
cat("===== CAPM 回歸分析 =====\n")
capm_results <- list()  # 用於存儲回歸結果

for (i in company) {
  y <- capm5[[i]] - capm5$riskfree  # 計算超額報酬
  x <- capm5$mkt - capm5$riskfree    # 市場風險溢酬

  # 執行線性回歸
  tab <- lm(y ~ x)

  # 存儲結果
  capm_results[[i]] <- summary(tab)

  # 顯示回歸結果
  cat("\nCompany:", i, "\n")
  cat("Intercept (Alpha):", coef(tab)[1], "\n")
  cat("Slope (Beta):", coef(tab)[2], "\n")
  print(summary(tab))
  cat("-------------------------\n")
}

# 顯示 GE 的數據
cat("===== GE Data Sample =====\n")
print(capm5[['ge']])

# 安裝並載入 ggplot2 以繪製回歸圖
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# 計算 Microsoft (MSFT) 的超額報酬與市場風險溢酬
y <- capm5[["msft"]] - capm5$riskfree
x <- capm5$mkt - capm5$riskfree
tab <- lm(y ~ x)

# 繪製 Microsoft 的 CAPM 回歸圖
ggplot(capm5, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.6) +  # 散點圖
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # 回歸線
  labs(title = "Microsoft CAPM",
       x = expression(r[m] - r[f]),  # 正確顯示數學符號
       y = expression(Microsoft ~ (r[j] - r[f]))) +
  theme_minimal() +
  theme(text = element_text(family = "Arial"))

cat("===== CAPM 分析完成 =====\n")

#假設截距項為零的β值
?lm
for (i in company) {
  y <- capm5[[i]] - capm5$riskfree
  x <- capm5$mkt - capm5$riskfree
  tab <- lm(y ~ x - 1)

  cat("Company:", i, "\n")
  cat("Slope:", coef(tab)[1], "\n")
  cat("-------------------------\n")
}


