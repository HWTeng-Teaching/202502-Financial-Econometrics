library(readr)
capm5 <- read_csv("r/econometric_r/capm5.csv")

#2.16.b
data <- capm5[, 3:9] - capm5$riskfree

coeff_values <- data.frame(Company = character(), Alpha = numeric(), Beta = numeric(), stringsAsFactors = FALSE)

for (company in colnames(data)[1:6]) {  # 只迭代各公司，不包含市場與風險利率
  model <- lm(data[[company]] ~ data$mkt)  # 進行線性回歸
  
  alpha <- coef(model)[1]  # Alpha (截距)
  beta <- coef(model)[2]   # Beta (市場風險係數)
  
  alpha_t <- summary(model)$coefficients[1, 3]  # Alpha 的 t 檢定值
  beta_t <- summary(model)$coefficients[2, 3]   # Beta 的 t 檢定值
  
  # 存入表格
  coeff_values <- rbind(coeff_values, 
                        data.frame(Company = company, Alpha = alpha, Beta = beta, 
                                   Alpha_t = alpha_t, Beta_t = beta_t))
}

rownames(coeff_values) <- NULL  # 重置 row names
coeff_values
# 2.16.c
plot(data$mkt,data$msft,
     ylim = c(0,max(data$msft)),
     xlim = c(0,max(data$mkt)),
     xlab = 'Rm-rf',
     ylab = 'Ri-rf',type = 'p')

modelmsft <- lm(data$msft ~ data$mkt)
abline(modelmsft, col = "red", lwd = 2)

# 2.16.d
# 初始化存儲 Beta 及其 t 檢定值的 DataFrame
coeff_values2 <- data.frame(Company = character(), Beta = numeric(), Beta_t = numeric(), stringsAsFactors = FALSE)

# 迴圈對每家公司進行無截距回歸
for (company in colnames(data)[1:6]) {  # 只迭代各公司，不包含市場與風險利率
  models <- lm(data[[company]] ~ data$mkt + 0)  # 無截距回歸
  
  beta2 <- coef(models)[1]   # Beta (唯一係數)
  beta_t2 <- summary(models)$coefficients[1, 3]  # Beta 的 t 檢定值 (修正錯誤)
  
  # 存入表格
  coeff_values2 <- rbind(coeff_values2, 
                         data.frame(Company = company, Beta = beta2, 
                                    Beta_t = beta_t2))
}

rownames(coeff_values2) <- NULL  # 重置 row names
print(coeff_values2)  # 顯示結果

  