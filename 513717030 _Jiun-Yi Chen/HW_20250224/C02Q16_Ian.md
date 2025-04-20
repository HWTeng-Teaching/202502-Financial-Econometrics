![圖片](https://github.com/user-attachments/assets/1bfc746b-97d6-462d-be3c-16f9b5e92f57)

## a.
The capital asset pricing model (CAPM):\
$r_j - r_f = \alpha_j + \beta_j (r_m - r_f) + \epsilon_j$
\
The model is a simple regression model because it can be written as $y = \beta_1 + \beta_2 x +e$

$y = r_j - r_f , x = r_m - r_f , \beta_1 = \alpha ,\beta_2 = \beta_j$
\
The capital asset pricing model (CAPM) is an important model in the field of finance. It explains variations in the rate of return on a security as a function of the rate of return on a portfolio consisting of all publicly traded stocks, which is called the *market portfolio*. Generally, the rate of return on any investment is measured relative to its opportunity cost, which is the return on a risk-free asset. The resulting difference is called the *risk premium*, since it is the reward or punishment for making a risky investment. The CAPM says that the risk premium in security \( j \) is *proportional* to the risk premium on the market portfolio. That is,

$r_j - r_f = \beta_j (r_m - r_f)$

where \( r_j \) and \( r_f \) are the returns to security \( j \) and the risk-free rate, respectively, \( r_m \) is the return on the market portfolio, and \( \beta_j \) is the security’s *beta* value. A stock’s *beta* is important to investors since it reveals the stock’s volatility. It measures the sensitivity of security \( j \)’s return to variation in the whole stock market. As such, values of \( \beta \) less than one indicate that the stock is *defensive* since its variation is less than the market’s. A beta greater than one indicates an *aggressive stock*. Investors usually want an estimate of a stock’s *beta* before purchasing it. The CAPM model shown above is the *economic model* in this case. The *econometric model* is obtained by including an intercept in the model (even though theory says it should be zero) and an error term:

## b
```
# Load necessary library
library(dplyr)

# Define firms
company <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# Initialize storage for results
results <- data.frame(
  Firm = character(), 
  Alpha = numeric(), Alpha_SE = numeric(),
  Beta = numeric(), Beta_SE = numeric(), 
  N = integer(),
  stringsAsFactors = FALSE
)

# Run CAPM regression for each firm
for (i in company) {
  y <- capm5[[i]] - capm5$riskfree  # Firm's excess return
  x <- capm5$mkt - capm5$riskfree   # Market excess return
  
  # Perform linear regression
  model <- lm(y ~ x)
  
  # Extract coefficients and standard errors
  alpha <- coef(model)[1]
  beta <- coef(model)[2]
  alpha_se <- summary(model)$coefficients[1,2]
  beta_se <- summary(model)$coefficients[2,2]
  
  # Store results
  results <- rbind(results, data.frame(
    Firm = i, Alpha = alpha, Alpha_SE = alpha_se,
    Beta = beta, Beta_SE = beta_se, N = length(y)
  ))
}

# Format table for readability
results <- results %>%
  mutate(
    Alpha = sprintf("%.6f", Alpha), 
    Alpha_SE = sprintf("SE(%.5f)", Alpha_SE),  # Fix: Make row names unique
    Beta = sprintf("%.3f", Beta),
    Beta_SE = sprintf("SE(%.4f)", Beta_SE)     # Fix: Make row names unique
  ) %>%
  select(-Firm)  # Remove Firm column before transposing

# Transpose the table (convert columns to rows)
results_t <- as.data.frame(t(results))
colnames(results_t) <- company  # Set firm names as column headers

# Set unique row names
rownames(results_t) <- c("b1=\u03B1̂j", "\u03B1(SE)",
                         "b2=\u03B2̂j", "\u03B2(SE)", "N")

# Print the final transposed table
print(results_t)
```
| Metric      | ge        | ibm      | ford     | msft     | dis      | xom      |
|:-----------|:---------|:--------|:--------|:--------|:--------|:--------|
| **b₁ = αⱼ**  | -0.000959 | 0.006053 | 0.003779 | 0.003250 | 0.001047 | 0.005284 |
| **SE(αⱼ)**   | (0.00442) | (0.00483) | (0.01023) | (0.00604) | (0.00468) | (0.00354) |
| **b₂ = βⱼ**  | 1.148     | 0.977    | 1.662    | 1.202    | 1.012    | 0.457    |
| **SE(βⱼ)**   | (0.0895)  | (0.0978)  | (0.2069)  | (0.1222)  | (0.0946)  | (0.0716)  |
| **N**        | 180       | 180      | 180      | 180      | 180      | 180      |

The stocks Ford, GE, and Microsoft are relatively aggressive with Ford being the most aggressive with a beta value of $\beta_2$ = 1.662. 

The others are relatively defensive with Exxon Mobil being the most defensive with a beta value of $\beta_2$ = 0.457.


** c.
```
# 安裝並載入 ggplot2（如果尚未安裝）
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 選擇要分析的公司
selected_company <- "msft"  # 可改成 "ge", "ibm", "ford", "dis", "xom"

# 計算超額報酬
capm5$Market_Excess <- capm5$mkt - capm5$riskfree
capm5$Stock_Excess <- capm5[[selected_company]] - capm5$riskfree

# 執行線性回歸（含截距）
capm_model <- lm(Stock_Excess ~ Market_Excess, data = capm5)

# 繪製回歸圖（確保顯示截距並加上箭頭指標）
ggplot(capm5, aes(x = Market_Excess, y = Stock_Excess)) +
  geom_point(alpha = 0.6, color = "brown") +  # 繪製散點圖
  geom_abline(intercept = coef(capm_model)[1], slope = coef(capm_model)[2], color = "green", size = 1.2) +  # 回歸線
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Y 軸 0 的虛線
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # X 軸 0 的虛線
  annotate("segment", x = min(capm5$Market_Excess), xend = max(capm5$Market_Excess) * 1.1, 
           y = 0, yend = 0, arrow = arrow(type = "closed", length = unit(0.07, "inches"))) +  # X軸箭頭
  annotate("segment", x = 0, xend = 0, 
           y = min(capm5$Stock_Excess), yend = max(capm5$Stock_Excess) * 1.1, 
           arrow = arrow(type = "closed", length = unit(0.07, "inches"))) +  # Y軸箭頭
  labs(title = paste("CAPM Regression for", toupper(selected_company)),
       subtitle = paste("Intercept (alpha) =", round(coef(capm_model)[1], 4),
                        ", Beta =", round(coef(capm_model)[2], 4)),
       x = "Market Excess Return",
       y = paste(toupper(selected_company), "Excess Return")) +
  theme_minimal()
```
Based on my observation, most of estimates of the α j are close to zero and are therefore consistent with finance theory. The fitted regression line and data scatter for Microsoft are plotted as below.
![C02Q16 C  picture](https://github.com/user-attachments/assets/623907e0-06d8-4a37-9302-2e7b56b9db96)


** d
```
# 從 GitHub 安裝 POE5Rdata 套件
install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata")
library(POE5Rdata)
data(capm5)
df <- capm5
str(df)

# Install ggplot2
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Define name of Companies
company <- c("ge", "ibm", "ford", "msft", "dis", "xom")

# 計算市場超額報酬
capm5$Market_Excess <- capm5$mkt - capm5$riskfree

# 儲存結果的資料框
beta_comparison <- data.frame(Company = company, Beta_With_Intercept = NA, Beta_No_Intercept = NA)

# 進行迴歸分析
for (i in company) {
  capm5$Stock_Excess <- capm5[[i]] - capm5$riskfree
  
  # 1. 估計 CAPM（包含截距）
  model_with_intercept <- lm(Stock_Excess ~ Market_Excess, data = capm5)
  
  # 2. 估計 CAPM（假設 α = 0）
  model_no_intercept <- lm(Stock_Excess ~ Market_Excess - 1, data = capm5)
  
  # 3. 存入結果
  beta_comparison[beta_comparison$Company == i, "Beta_With_Intercept"] <- coef(model_with_intercept)[2]
  beta_comparison[beta_comparison$Company == i, "Beta_No_Intercept"] <- coef(model_no_intercept)[1]
}

# 顯示 beta 值比較結果
print(beta_comparison)

# 計算 beta 變化程度
beta_comparison$Beta_Change <- beta_comparison$Beta_No_Intercept - beta_comparison$Beta_With_Intercept

##nnnnn
# 重新計算 Beta 差異：取絕對值，並保留 4 位小數，不進位
beta_comparison$Beta_Change <- abs(beta_comparison$Beta_No_Intercept - beta_comparison$Beta_With_Intercept)
beta_comparison$Beta_Change <- floor(beta_comparison$Beta_Change * 10000) / 10000  # 不進位，保留4位

# 畫出 Beta 差異的柱狀圖
ggplot(beta_comparison, aes(x = Company, y = Beta_Change, fill = Company)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.4f", Beta_Change)), vjust = -0.5, size = 4) +
  labs(
    title = "各家公司 Beta 差異（|無截距 - 有截距|）",
    x = "公司",
    y = "Beta 差異值"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```
![Rplot01](https://github.com/user-attachments/assets/b7410f6b-ffae-48e7-9aca-09045d81f826)

