url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"
download.file(url, destfile = "motel.rdata", mode = "wb")  # 下載檔案

# 1. Load data
load("motel.rdata")

str(motel)  # 查看結構
head(motel)  # 查看前幾筆數據


# Base plot for MOTEL_PCT
plot(motel$time, motel$motel_pct, type = "l", col = "blue", ylim = c(0, 100),
     xlab = "Time", ylab = "Occupancy Rates (%)", main = "Motel vs Competitor Occupancy Rates Over Time")

# Add COMP_PCT
lines(motel$time, motel$comp_pct, col = "red")

# Add a legend
legend("bottomright", legend = c("MOTEL_PCT", "COMP_PCT"), col = c("blue", "red"), lty = 1)

model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

confint(model, level = 0.95)

# New data
new_data <- data.frame(comp_pct = 70)

# Predict with 90% confidence interval
predict(model, newdata = new_data, interval = "confidence", level = 0.90)

summary(model)

# Manually calculate the t-statistic
beta2_est <- coef(model)["comp_pct"]
se_beta2 <- summary(model)$coefficients["comp_pct", "Std. Error"]
t_value <- beta2_est / se_beta2

# Degrees of freedom
df <- df.residual(model)
df
# Critical value for α = 0.01, one-tailed
t_critical <- qt(0.99, df)

# Compare
t_value > t_critical  # TRUE = reject H₀
t_value
t_critical

t_value_diff1 <- (beta2_est - 1) / se_beta2

# Two-tailed critical values for α = 0.01
t_critical2 <- qt(0.995, df)
t_critical2
# Rejection region: |t| > t_critical2
abs(t_value_diff1) > t_critical2  # TRUE = reject H₀
t_value_diff1


residuals <- resid(model)

# Plot residuals over TIME
plot(motel$time, residuals, type = "o", col = "purple",
     xlab = "Time", ylab = "Residuals", main = "Residuals Over Time")

abline(h = 0, lty = 2)
