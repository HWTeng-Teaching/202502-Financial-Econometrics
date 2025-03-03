# c
# Yes. All alpha parameters are close to zero.

library(ggplot2)

ggplot(capm5, aes(x = mkt, y = msft)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "CAPM Regression for Microsoft", 
       x = "Market Excess Return (MKT - RISKFREE)", 
       y = "Microsoft Excess Return")


# d

beta_values_no_alpha <- data.frame(Company=character(), Beta=numeric(), stringsAsFactors=FALSE)

for (company in companies) {
  model_no_alpha <- lm(capm5[[company]] ~ capm5$mkt - 1)  
  summary_model_no_alpha <- summary(model_no_alpha)
  
  beta_values_no_alpha <- rbind(beta_values_no_alpha, data.frame(
    Company = company,
    Beta = summary_model_no_alpha$coefficients[1, 1]
  ))
}

print(beta_values_no_alpha)
# Company      Beta
# 1      ge 1.1467633
# 2     ibm 0.9843954
# 3    ford 1.6667168
# 4    msft 1.2058695
# 5     dis 1.0128190
# 6     xom 0.4630727
print(beta_values[, c("Company", "Beta")])
# Company      Beta
# 1      ge 1.1479521
# 2     ibm 0.9768898
# 3    ford 1.6620307
# 4    msft 1.2018398
# 5     dis 1.0115207
# 6     xom 0.4565208



# Not change much.