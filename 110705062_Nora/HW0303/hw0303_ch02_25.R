# Download and load the dataset
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"
file_path <- "cex5_small.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(cex5_small)

## 2.25.1 Histogram
hist(cex5_small$foodaway, 
     main = "Histogram of foodaway", 
     xlab = "Expenditure on Food Away from Home", 
     col = "lightblue", 
     border = "black", 
     breaks = 100) 

summary_stats <- summary(cex5_small$foodaway)
mean_value <- mean(cex5_small$foodaway, na.rm = TRUE)
median_value <- median(cex5_small$foodaway, na.rm = TRUE)
quantiles <- quantile(cex5_small$foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

print(summary_stats)
cat("Mean:", mean_value, "\n")
cat("Median:", median_value, "\n")
cat("25th percentile:", quantiles[1], "\n")
cat("75th percentile:", quantiles[2], "\n")

## 2.25.2 Mean and Median Value
mean_advanced <- mean(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)
median_advanced <- median(cex5_small$foodaway[cex5_small$advanced == 1], na.rm = TRUE)
mean_college <- mean(cex5_small$foodaway[cex5_small$college == 1 & cex5_small$advanced == 0], na.rm = TRUE)
median_college <- median(cex5_small$foodaway[cex5_small$college == 1 & cex5_small$advanced == 0], na.rm = TRUE)
mean_no_degree <- mean(cex5_small$foodaway[cex5_small$college == 0 & cex5_small$advanced == 0], na.rm = TRUE)
median_no_degree <- median(cex5_small$foodaway[cex5_small$college == 0 & cex5_small$advanced == 0], na.rm = TRUE)

cat("Households with an advanced degree member:\nMean:", mean_advanced, "\nMedian:", median_advanced, "\n\n")
cat("Households with a college degree member (but no advanced degree):\nMean:", mean_college, "\nMedian:", median_college, "\n\n")
cat("Households with no advanced or college degree member:\nMean:", mean_no_degree, "\nMedian:", median_no_degree, "\n")

## 2.25.3 ln(foodway)
cex5_cleaned <- cex5_small[cex5_small$foodaway > 0, ]
num_removed <- nrow(cex5_small) - nrow(cex5_cleaned)
cat("Number of observations removed before taking ln(FOODAWAY):", num_removed, "\n")
cex5_cleaned$log_foodaway <- log(cex5_cleaned$foodaway)

hist(cex5_cleaned$log_foodaway, 
     main = "Histogram of ln(FOODAWAY)", 
     xlab = "ln(Expenditure on Food Away from Home)", 
     col = "lightblue", 
     border = "black", 
     breaks = 100)

summary_log_foodaway_cleaned <- summary(cex5_cleaned$log_foodaway)
mean_log_foodaway_cleaned <- mean(cex5_cleaned$log_foodaway, na.rm = TRUE)
median_log_foodaway_cleaned <- median(cex5_cleaned$log_foodaway, na.rm = TRUE)
quantiles_log_foodaway_cleaned <- quantile(cex5_cleaned$log_foodaway, probs = c(0.25, 0.75), na.rm = TRUE)

print(summary_log_foodaway_cleaned)
cat("Mean of ln(FOODAWAY):", mean_log_foodaway_cleaned, "\n")
cat("Median of ln(FOODAWAY):", median_log_foodaway_cleaned, "\n")
cat("25th percentile of ln(FOODAWAY):", quantiles_log_foodaway_cleaned[1], "\n")
cat("75th percentile of ln(FOODAWAY):", quantiles_log_foodaway_cleaned[2], "\n")

## 2.25.4 Regression Model
reg_model <- lm(log_foodaway ~ income, data = cex5_cleaned)
summary(reg_model)
slope_estimate <- coef(reg_model)["income"]
cat("Estimated slope (β2):", slope_estimate, "\n")

## 2.25.5 Scatter plot
plot(cex5_cleaned$income, cex5_cleaned$log_foodaway,
     main = "Scatter Plot of ln(FOODAWAY) vs INCOME",
     xlab = "Income",
     ylab = "ln(Expenditure on Food Away from Home)",
     col = "blue", 
     pch = 16, 
     cex = 0.7)

abline(reg_model, col = "red", lwd = 2)
summary(reg_model)

## 2.25.6 Residuals
cex5_cleaned$residuals <- resid(reg_model)

plot(cex5_cleaned$income, cex5_cleaned$residuals,
     main = "Residuals vs INCOME",
     xlab = "Income",
     ylab = "Residuals",
     col = "blue", 
     pch = 16, 
     cex = 0.7)

