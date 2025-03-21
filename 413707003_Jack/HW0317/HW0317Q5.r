# 4.29
# a
library(moments)
library(ggplot2)

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"
load(url(url))

summary_stats <- function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE))
}

food_stats <- summary_stats(cex5_small$food)
income_stats <- summary_stats(cex5_small$income)

food_stats
#     Mean   Median      Min      Max       SD 
# 114.4431  99.8000   9.6300 476.6700  72.6575 


income_stats
#     Mean    Median       Min       Max        SD 
# 72.14264  65.29000  10.00000 200.00000  41.65228 


hist_plot <- function(var, title) {
  hist_data <- hist(var, breaks = 30, col = "lightblue", main = title, xlab = title, probability = TRUE)
  
  mean_val <- mean(var, na.rm = TRUE)
  median_val <- median(var, na.rm = TRUE)
  
  density_data <- density(var, na.rm = TRUE)
  y_max <- max(density_data$y) * 0.9  
  
  abline(v = mean_val, col = "red", lwd = 2, lty = 2)  
  abline(v = median_val, col = "blue", lwd = 2, lty = 2) 
  
  text(mean_val, y_max, paste("Mean =", round(mean_val, 2)), col = "red", pos = 4, cex = 1.2)
  text(median_val, y_max * 0.85, paste("Median =", round(median_val, 2)), col = "blue", pos = 2, cex = 1.2)
}

par(mfrow = c(1, 2))
hist_plot(cex5_small$food, "FOOD histogram")
hist_plot(cex5_small$income, "INCOME histogram")


jb_food <- jarque.test(cex5_small$food)
jb_income <- jarque.test(cex5_small$income)

jb_food
# Jarque-Bera Normality Test
# 
# data:  cex5_small$food
# JB = 648.65, p-value < 2.2e-16
# alternative hypothesis: greater


jb_income
# Jarque-Bera Normality Test
# 
# data:  cex5_small$income
# JB = 148.21, p-value < 2.2e-16
# alternative hypothesis: greater

#Both variables do not follow a normal distribution.


# b

cex5_small$log_food <- log(cex5_small$food)
cex5_small$log_income <- log(cex5_small$income)

lm_model <- lm(food ~ income, data = cex5_small)
log_log_model <- lm(log_food ~ log_income, data = cex5_small)
linear_log_model <- lm(food ~ log_income, data = cex5_small)

ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "FOOD vs INCOME", x = "INCOME", y = "FOOD")

confint(lm_model, "income", level = 0.95)
#    2.5 %   97.5 %
# 0.2619215 0.455452

# confident interval [0.2619215, 0.455452]

summary(lm_model)$r.squared
# 0.0422812
summary(log_log_model)$r.squared
# 0.03322915
summary(linear_log_model)$r.squared
# 0.03799984

# Linear model is best in three models, but I do not think so because the R^2 value is too low.








