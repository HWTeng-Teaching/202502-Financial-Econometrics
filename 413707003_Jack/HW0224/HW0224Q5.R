# a 
# CAPM: r_j - r_f = alpha_j + beta_j (r_m - r_f) + e_j
# simple regression model: y = beta_1 + beta_2 (x) + e
# These two equations have the same patterns, so we can identified CAPM model as simple regression model.




# b

companies <- c("ge", "ibm", "ford", "msft", "dis", "xom")


for (company in companies) {
  capm5[[company]] <- capm5[[company]] - capm5$riskfree
}
capm5$mkt <- capm5$mkt - capm5$riskfree


beta_values <- data.frame(Company=character(), Beta=numeric(), Alpha=numeric(), Alpha_p=numeric(), stringsAsFactors=FALSE)

for (company in companies) {
  model <- lm(capm5[[company]] ~ capm5$mkt)
  summary_model <- summary(model)
  
  beta_values <- rbind(beta_values, data.frame(
    Company = company,
    Beta = summary_model$coefficients[2, 1],
    Alpha = summary_model$coefficients[1, 1],
    Alpha_p = summary_model$coefficients[1, 4]
  ))
}


most_aggressive <- beta_values[which.max(beta_values$Beta), ]
most_defensive <- beta_values[which.min(beta_values$Beta), ]


print(most_aggressive)
#   Company     Beta       Alpha   Alpha_p
# 3    ford 1.662031 0.003778911 0.7121467

print(most_defensive)
#   Company      Beta       Alpha   Alpha_p
# 6     xom 0.4565208 0.005283533 0.1368343

print(beta_values)

# Company      Beta         Alpha   Alpha_p
# 1      ge 1.1479521 -0.0009586682 0.8287072
# 2     ibm 0.9768898  0.0060525497 0.2122303
# 3    ford 1.6620307  0.0037789112 0.7121467
# 4    msft 1.2018398  0.0032496009 0.5909844
# 5     dis 1.0115207  0.0010469237 0.8231091
# 6     xom 0.4565208  0.0052835329 0.1368343

