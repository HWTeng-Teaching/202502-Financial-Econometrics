#2.25 (a)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)

library(POE5Rdata)
data("cex5_small")

hist(cex5_small$foodaway,
     main = "Histogram of FOODAWAY",
     xlab = "FOODAWAY (Monthly Expenditure on Food Away from Home)",
     col = "purple", border = "black")

summary(cex5_small$foodaway)

mean_foodaway <- mean(cex5_small$foodaway, na.rm = TRUE)
median_foodaway <- median(cex5_small$foodaway, na.rm = TRUE)

mean_foodaway
median_foodaway

quantile(cex5_small$foodaway, probs = c(0.25, 0.75))

#2.25(b)
#advanced degree
advanced_data <- cex5_small[cex5_small$advanced == 1, ]

advanced_group <- data.frame(
  mean_foodaway = mean(advanced_data$foodaway, na.rm = TRUE),
  median_foodaway = median(advanced_data$foodaway, na.rm = TRUE),
  count = nrow(advanced_data))

advanced_group

#college degree
college_data <- cex5_small[cex5_small$advanced == 0 & cex5_small$college == 1, ]

college_group <- data.frame(
  mean_foodaway = mean(college_data$foodaway, na.rm = TRUE),
  median_foodaway = median(college_data$foodaway, na.rm = TRUE),
  count = nrow(college_data))

college_group

#no advanced or college degree
no_degree_data <- cex5_small[cex5_small$advanced == 0 & cex5_small$college == 0, ]

no_degree_group <- data.frame(
  mean_foodaway = mean(no_degree_data$foodaway, na.rm = TRUE),
  median_foodaway = median(no_degree_data$foodaway, na.rm = TRUE),
  count = nrow(no_degree_data))

no_degree_group

#2.25(c)
cex5_small_pos <- cex5_small[cex5_small$foodaway > 0, ]
ln_FOODAWAY <- log(cex5_small_pos$foodaway)

hist(ln_FOODAWAY,
     main = "Histogram of ln(FOODAWAY)",
     xlab = "ln(FOODAWAY)",
     col = "purple", border = "black")

summary(ln_FOODAWAY)

sum(is.finite(cex5_small$foodaway)) 
sum(is.finite(ln_FOODAWAY)) 

#2.25(d)
y = ln_FOODAWAY
x_income = cex5_small_pos$income
lm_model <- lm(y ~ x_income)

summary(lm_model)

#2.25(e)
plot(x_income, y,
     main = "ln(FOODAWAY) vs INCOME (FOODAWAY > 0)",
     xlab = "INCOME (Household Monthly Income, $100 units)",
     ylab = "ln(FOODAWAY)",
     pch = 19, col = "purple")

abline(lm_model, col = "pink", lwd = 2)

#2.25(f)
residuals_lm <- resid(lm_model)

plot(x_income, residuals_lm,
     main = "Residuals vs INCOME",
     xlab = "INCOME (Household Monthly Income, $100 units)",
     ylab = "Residuals",
     pch = 19, col = "purple")

abline(h = 0, col = "orange",lwd=5, lty = 2)
