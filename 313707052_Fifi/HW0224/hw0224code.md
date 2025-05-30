
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)


library(POE5Rdata)
data("capm5")


company <- c("ge", "ibm", "ford", "msft", "dis", "xom")


cat("===== CAPM 回歸分析 =====\n")
capm_results <- list()  

for (i in company) {
  y <- capm5[[i]] - capm5$riskfree  
  x <- capm5$mkt - capm5$riskfree    
  

  tab <- lm(y ~ x)
  

  capm_results[[i]] <- summary(tab)
  
  
  cat("\nCompany:", i, "\n")
  cat("Intercept (Alpha):", coef(tab)[1], "\n")
  cat("Slope (Beta):", coef(tab)[2], "\n")
  print(summary(tab))
  cat("-------------------------\n")
}


cat("===== GE Data Sample =====\n")
print(capm5[['ge']])


if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)


y <- capm5[["msft"]] - capm5$riskfree
x <- capm5$mkt - capm5$riskfree
tab <- lm(y ~ x)


ggplot(capm5, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Microsoft CAPM",
       x = expression(r[m] - r[f]),  
       y = expression(Microsoft ~ (r[j] - r[f]))) +  
  theme_minimal() +
  theme(text = element_text(family = "Arial"))

cat("===== CAPM 分析完成 =====\n")


?lm
for (i in company) {
  y <- capm5[[i]] - capm5$riskfree
  x <- capm5$mkt - capm5$riskfree
  tab <- lm(y ~ x - 1)
  
  cat("Company:", i, "\n")
  cat("Slope:", coef(tab)[1], "\n")
  cat("-------------------------\n")
}
