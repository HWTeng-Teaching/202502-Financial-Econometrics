url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"
download.file(url, destfile = "cex5_small.rdata", mode = "wb")  # 下載檔案

# 1. Load data
load("cex5_small.rdata")

str(cex5_small)  # 查看結構
head(cex5_small)  # 查看前幾筆數據
#head(cex5_small, n = 20) 
# Basic summary statistics for FOODAWAY
summary(cex5_small$foodaway)

# More detailed statistics
mean_foodaway <- mean(cex5_small$foodaway)
median_foodaway <- median(cex5_small$foodaway)
quantiles_foodaway <- quantile(cex5_small$foodaway, probs = c(0.25, 0.75))

# Print the results
mean_foodaway
median_foodaway
quantiles_foodaway

# Histogram of FOODAWAY
ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(binwidth = 20, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of FOODAWAY Expenditure",
       x = "Food Away from Home Expenditure ($)",
       y = "Frequency") +
  theme_minimal()


#legend("topright", legend = c("Mean", "Median"), col = c("red", "blue"),
#       lwd = 2, lty = 2)


# Group 1: Households with an advanced degree member
adv_degree <- subset(cex5_small, advanced == 1)

# Group 2: Households with a college degree member
college_degree <- subset(cex5_small, college == 1)

# Group 3: Households with no advanced or college degree member
no_degree <- subset(cex5_small, advanced == 0 & college == 0)

mean(adv_degree$foodaway)
median(adv_degree$foodaway)

mean(college_degree$foodaway)
median(college_degree$foodaway)

mean(no_degree$foodaway)
median(no_degree$foodaway)


cex5_small$ln_foodaway <- log(cex5_small$foodaway)
summary(cex5_small$ln_foodaway)

# Histogram of ln(FOODAWAY)
ggplot(cex5_small, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of ln(FOODAWAY)",
       x = "ln(Food Away from Home Expenditure)",
       y = "Frequency") +
  theme_minimal()

# How many FOODAWAY are zero?
sum(cex5_small$foodaway == 0)

cex5_small_nonzero <- subset(cex5_small, foodaway > 0)

# Create a new variable for ln(FOODAWAY) on the filtered data
cex5_small_nonzero$ln_foodaway <- log(cex5_small_nonzero$foodaway)

# Summary statistics for ln(FOODAWAY)
summary(cex5_small_nonzero$ln_foodaway)

# Histogram of ln(FOODAWAY) excluding zero values
ggplot(cex5_small_nonzero, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.2, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of ln(FOODAWAY) (Excluding Zero Values)",
       x = "ln(Food Away from Home Expenditure)",
       y = "Frequency") +
  theme_minimal()

model <- lm(ln_foodaway ~ income, data = cex5_small_nonzero)

# View the summary results
summary(model)

library(ggplot2)

ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(alpha = 0.5, color = "blue") +  # scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +  # regression line
  labs(title = "ln(FOODAWAY) vs INCOME with Regression Line",
       x = "Household Income (in $100 units)",
       y = "ln(Food Away from Home Expenditure)") +
  theme_minimal()

ggplot(cex5_small_nonzero, aes(x = income, y = ln_foodaway)) +
  geom_point(alpha = 0.5, color = "blue") +  # scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 1) +  # regression line
  labs(title = "ln(FOODAWAY) vs INCOME with Regression Line",
       x = "Household Income (in $100 units)",
       y = "ln(Food Away from Home Expenditure)") +
  theme_minimal()

cex5_small_nonzero$residuals <- resid(model)


ggplot(cex5_small_nonzero, aes(x = income, y = residuals)) +
  geom_point(alpha = 0.5, color = "blue") +  # 散點圖
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # 殘差為0的水平線
  labs(title = "Residuals vs INCOME",
       x = "Household Income (in $100 units)",
       y = "Residuals from ln(FOODAWAY) Regression") +
  theme_minimal()




