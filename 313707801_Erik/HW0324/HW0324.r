#Homework 0324
#Question 5.23
#Part a
setwd('/Users/erikparkhomenko/Library/Mobile Documents/com~apple~CloudDocs/Studies/NYCU/Financial Econometrics/Data/poe5rdata')
load("cocaine.rdata")

cokeprice <- lm(price ~ quant + qual + trend, data = cocaine)
summary(cokeprice)

#Part d
# Extract coefficient and standard error for QUANT
b2 <- coef(cokeprice)["quant"]
se_b2 <- summary(cokeprice)$coefficients["quant", "Std. Error"]

# Calculate t-statistic for one-sided test
t_value <- (b2 - 0) / se_b2
df <- df.residual(cokeprice)

# Calculate p-value for left-tail test
p_value <- pt(t_value, df)

# Output the results
cat("b2 (quant):", round(b2, 5), "\n")
cat("Standard Error:", round(se_b2, 5), "\n")
cat("t-value:", round(t_value, 3), "\n")
cat("p-value:", round(p_value, 5), "\n")

# Part e

# Extract coefficient and standard error for QUAL
b3 <- coef(cokeprice)["qual"]
se_b3 <- summary(cokeprice)$coefficients["qual", "Std. Error"]

# Calculate t-statistic for right-tail test
t_value <- b3 / se_b3
df <- df.residual(cokeprice)

# Calculate p-value for right-tail test
p_value <- 1 - pt(t_value, df)

# Output results
cat("b3 (qual):", round(b3, 5), "\n")
cat("Standard Error:", round(se_b3, 5), "\n")
cat("t-value:", round(t_value, 3), "\n")
cat("p-value:", round(p_value, 5), "\n")