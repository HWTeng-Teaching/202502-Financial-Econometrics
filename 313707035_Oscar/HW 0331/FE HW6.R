# 載入資料與套件
library(POE5Rdata)
data("commute5")


# Fit the linear model
model <- lm(time ~ depart + reds + trains, data = commute5)

# Display the summary of the model
summary(model)

confint(model, level = 0.95)

# Given estimates:
b3 <- 1.5219
se3 <- 1.96

# Compute the t-statistic:
# t = (estimate - hypothesized value) / SE
t_stat <- (b3 - 2) / se3
cat("t-statistic =", t_stat, "\n")

# If you have the model, you can extract the residual degrees of freedom.
# For demonstration, let's assume df = 245 (adjust as appropriate):
df <- 245

# Compute the one-sided p-value for a left-tail test (beta < 2)
p_value <- pt(t_stat, df = df, lower.tail = TRUE)
cat("One-sided p-value =", p_value, "\n")

# Estimated coefficient for trains:
b4 <- 3.0237

# Approximated standard error:
se4 <- 0.6244

# Hypothesized value under H0:
beta0 <- 3

# Compute the t-statistic:
t_stat <- (b4 - beta0) / se4
cat("t-statistic =", t_stat, "\n")

# Degrees of freedom (adjust as per your model)
df <- 245

# Compute the two-sided p-value:
p_value <- 2 * pt(-abs(t_stat), df = df)
cat("Two-sided p-value =", p_value, "\n")

# Decision at 10% significance level:
alpha <- 0.10
if(p_value < alpha){
  cat("Reject the null hypothesis at the 10% significance level.\n")
} else {
  cat("Do not reject the null hypothesis at the 10% significance level.\n")
}

# Create the dummy variable (assuming depart is in minutes after 6:30 AM)
commute5$after730 <- ifelse(commute5$depart > 60, 1, 0)

# Fit the augmented model
model2 <- lm(time ~ depart + reds + trains + after730, data = commute5)
summary(model2)

# Extract the after730 coefficient and its standard error from model2
b5 <- coef(model2)["after730"]
se5 <- sqrt(vcov(model2)["after730", "after730"])
df_model2 <- df.residual(model2)

# Compute t-statistic for testing if after730 effect < 10 minutes
t_stat <- (b5 - 10) / se5

# One-sided p-value (left-tail test)
p_value <- pt(t_stat, df = df_model2, lower.tail = TRUE)

# Output the t-statistic and p-value
t_stat
p_value

model <- lm(time ~ depart + reds + trains, data = commute5)
# Extract coefficient estimates for 'reds' and 'trains'
b_reds   <- coef(model)["reds"]
b_trains <- coef(model)["trains"]

# Compute the linear combination: L_hat = beta_trains - 3 * beta_reds
L_hat <- b_trains - 3 * b_reds

# Extract the variance-covariance matrix from the model
vcov_mat <- vcov(model)

# Compute the variance of L_hat:
# Var(L_hat) = Var(beta_trains) + 9*Var(beta_reds) - 6*Cov(beta_trains, beta_reds)
var_trains <- vcov_mat["trains", "trains"]
var_reds   <- vcov_mat["reds", "reds"]
cov_trains_reds <- vcov_mat["trains", "reds"]

var_L <- var_trains + 9 * var_reds - 6 * cov_trains_reds

# Standard error of L_hat:
se_L <- sqrt(var_L)

# Compute the t-statistic for the test H0: L >= 0 vs. H1: L < 0
t_stat <- L_hat / se_L

# Degrees of freedom from the model
df_model <- df.residual(model)

# One-sided p-value (lower-tail, since H1 is L < 0)
p_value <- pt(t_stat, df = df_model, lower.tail = TRUE)

# Print the results
cat("Estimated L (beta_trains - 3 * beta_reds) =", L_hat, "\n")
cat("Standard error =", se_L, "\n")
cat("t-statistic =", t_stat, "\n")
cat("One-sided p-value =", p_value, "\n")

# Decision at alpha = 0.05:
alpha <- 0.05
if(p_value < alpha){
  cat("Reject H0: There is evidence that the expected train delay is less than 3 times the red light delay.\n")
} else {
  cat("Do not reject H0: Insufficient evidence that the expected train delay is less than 3 times the red light delay.\n")
}


model <- lm(time ~ depart + reds + trains, data = commute5)
new_data <- data.frame(depart = 30, reds = 6, trains = 1)
# Get the predicted value and 95% confidence interval for the mean prediction
pred <- predict(model, newdata = new_data, interval = "confidence", level = 0.95)
# 'pred' is a matrix with columns: fit, lwr, and upr.
L_hat <- pred[1, "fit"]  # Estimated mean travel time
# Get degrees of freedom from the model
df_model <- df.residual(model)
t_crit <- qt(0.975, df_model)

# Estimate the standard error from the width of the upper bound
SE_L <- (pred[1, "upr"] - L_hat) / t_crit

# Compute the t-statistic for testing H0: E(time|X) <= 45 vs. H1: E(time|X) > 45
t_stat <- (L_hat - 45) / SE_pred
cat("t-statistic =", t_stat, "\n")

# One-sided p-value (upper tail)
p_value <- pt(t_stat, df = df_model, lower.tail = FALSE)
cat("One-sided p-value =", p_value, "\n")

# Decision at alpha = 0.05
alpha <- 0.05
if(p_value < alpha){
  cat("Reject H0: There is evidence that leaving at 7:00 AM is not early enough (expected travel time > 45 minutes).\n")
} else {
  cat("Do not reject H0: There is insufficient evidence to conclude that leaving at 7:00 AM is not early enough (expected travel time <= 45 minutes).\n")
}

#(h) The original hypothesis setup is correct because it is framed around the primary concern: ensuring Bill is not late.


##5.33(a)
data("cps5_small")
data <- cps5_small
model <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ * exper), data = cps5_small)

summary(model)

##(b)
marginal_effect <- function(educ, exper, coef) {
  return(coef["educ"] + 2 * coef["I(educ^2)"] * educ + coef["I(educ * exper)"] * exper)
}

educ_values <- c(12, 16)  # High school vs. college graduate
exper_values <- c(5, 20)   # Young vs. experienced worker

for (educ in educ_values) {
  for (exper in exper_values) {
    effect <- marginal_effect(educ, exper, coef(model))
    cat("Marginal Effect at EDUC =", educ, "and EXPER =", exper, ":", effect, "\n")
  }
}

##(c)
library(ggplot2)

b2 <- coef(model)["educ"]
b3 <- coef(model)["I(educ^2)"]
b6 <- coef(model)["I(educ * exper)"]

data$marginal_effect <- b2 + 2 * b3 * data$educ + b6 * data$exper

ggplot(data, aes(x = marginal_effect)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = median(marginal_effect)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(marginal_effect, 0.05)), color = "green", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(marginal_effect, 0.95)), color = "purple", linetype = "dashed") +
  labs(title = "Histogram of Marginal Effects of Education on Log-Wage",
       x = "Marginal Effect",
       y = "Frequency")
median_effect <- median(data$marginal_effect)
percentile_5 <- quantile(data$marginal_effect, 0.05)
percentile_95 <- quantile(data$marginal_effect, 0.95)

cat("Median Marginal Effect:", median_effect, "\n")
cat("5th Percentile:", percentile_5, "\n")
cat("95th Percentile:", percentile_95, "\n")
## It looks like the marginal effect is a little bit skewed.

##(d)
##β4+2β5*EXPER+β6*EDUC
# ∂E[ln(WAGE)]/∂EXPER = β4 + 2 * β5 * EXPER + β6 * EDUC
marginal_effect_exper <- function(educ, exper) {
  coefs["exper"] + 2 * coefs["I(exper^2)"] * exper + coefs["I(educ * exper)"] * educ
}


##(e)
# Load necessary packages
library(ggplot2)

# Assuming you have already estimated the model and stored it as "model"
coefficients <- coef(model)  # Extract coefficients

# Compute the marginal effect for each observation
cps5_small$marginal_effect <- coefficients["exper"] + 
  2 * coefficients["I(exper^2)"] * cps5_small$exper + 
  coefficients["I(educ * exper)"] * cps5_small$educ


# Histogram of marginal effects
ggplot(cps5_small, aes(x = marginal_effect)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "blue", alpha = 0.7) +
  geom_vline(aes(xintercept = median(marginal_effect)), color = "red", linetype = "dashed") +
  labs(title = "Histogram of Marginal Effects",
       x = "Marginal Effect of Experience",
       y = "Frequency")

# Compute key percentiles
quantiles <- quantile(cps5_small$marginal_effect, probs = c(0.05, 0.50, 0.95))
print(quantiles)

##(f)
# Define the values for David and Svetlana
EDUC_D <- 17
EXPER_D <- 8
EDUC_S <- 16
EXPER_S <- 18

# Extract coefficients from the estimated model
coefficients <- coef(model)

# Compute predicted log-wages
ln_WAGE_D <- coefficients["(Intercept)"] + coefficients["educ"] * EDUC_D + 
  coefficients["I(educ^2)"] * (EDUC_D)^2 + coefficients["exper"] * EXPER_D + 
  coefficients["I(exper^2)"] * (EXPER_D)^2 + coefficients["I(educ * exper)"] * (EDUC_D * EXPER_D)

ln_WAGE_S <- coefficients["(Intercept)"] + coefficients["educ"] * EDUC_S + 
  coefficients["I(educ^2)"] * EDUC_S^2 + coefficients["exper"] * EXPER_S + 
  coefficients["I(exper^2)"] * EXPER_S^2 + coefficients["I(educ * exper)"] * (EDUC_S * EXPER_S)

# Compute the difference
D <- ln_WAGE_D - ln_WAGE_S

# Compute standard error of difference
vcov_matrix <- vcov(model)  # Variance-covariance matrix
grad_D <- c(1, EDUC_D, EDUC_D^2, EXPER_D, EXPER_D^2, EDUC_D * EXPER_D) - 
  c(1, EDUC_S, EDUC_S^2, EXPER_S, EXPER_S^2, EDUC_S * EXPER_S)

SE_D <- sqrt(t(grad_D) %*% vcov_matrix %*% grad_D)

# Compute t-statistic
t_stat <- D / SE_D

# Compute p-value for one-tailed test
p_value <- pt(t_stat, df = df.residual(model), lower.tail = FALSE)

# Display results
cat("Difference in log-wages:", D, "\n")
cat("Standard error of difference:", SE_D, "\n")
cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

# Decision rule
if (p_value < 0.05) {
  cat("Reject H0: David's expected log-wage is significantly greater than Svetlana's.\n")
} else {
  cat("Fail to reject H0: No significant evidence that David's expected log-wage is greater than Svetlana's.\n")
}

##(g)
# Update experience values
EXPER_D_new <- 16
EXPER_S_new <- 26

# Compute new predicted log-wages
ln_WAGE_D_new <- coefficients["(Intercept)"] + coefficients["educ"] * EDUC_D + 
  coefficients["I(educ^2)"] * EDUC_D^2 + coefficients["exper"] * EXPER_D_new + 
  coefficients["I(exper^2)"] * EXPER_D_new^2 + coefficients["I(educ * exper)"] * (EDUC_D * EXPER_D_new)

ln_WAGE_S_new <- coefficients["(Intercept)"] + coefficients["educ"] * EDUC_S + 
  coefficients["I(educ^2)"] * EDUC_S^2 + coefficients["exper"] * EXPER_S_new + 
  coefficients["I(exper^2)"] * EXPER_S_new^2 + coefficients["I(educ * exper)"] * (EDUC_S * EXPER_S_new)

# Compute new difference
D_new <- ln_WAGE_D_new - ln_WAGE_S_new

# Compute standard error of difference
grad_D_new <- c(1, EDUC_D, EDUC_D^2, EXPER_D_new, EXPER_D_new^2, EDUC_D * EXPER_D_new) - 
  c(1, EDUC_S, EDUC_S^2, EXPER_S_new, EXPER_S_new^2, EDUC_S * EXPER_S_new)

SE_D_new <- sqrt(t(grad_D_new) %*% vcov_matrix %*% grad_D_new)

# Compute new t-statistic
t_stat_new <- D_new / SE_D_new

# Compute new p-value
p_value_new <- pt(t_stat_new, df = df.residual(model), lower.tail = FALSE)

# Display results
cat("New difference in log-wages:", D_new, "\n")
cat("New standard error of difference:", SE_D_new, "\n")
cat("New t-statistic:", t_stat_new, "\n")
cat("New p-value:", p_value_new, "\n")

# Decision rule
if (p_value_new < 0.05) {
  cat("Reject H0: David's expected log-wage is still significantly greater than Svetlana's.\n")
} else {
  cat("Fail to reject H0: No significant evidence that David's expected log-wage is greater.\n")
}

##(h)
# Extract coefficient estimates
beta5_hat <- coef(model)["I(exper^2)"]
beta6_hat <- coef(model)["I(educ * exper)"]

# Compute estimated difference
D_hat <- 12 * beta5_hat - 4 * beta6_hat

# Extract variance and covariance from the model's variance-covariance matrix
vcov_matrix <- vcov(model)
var_beta5 <- vcov_matrix["I(exper^2)", "I(exper^2)"]
var_beta6 <- vcov_matrix["I(educ * exper)", "I(educ * exper)"]
cov_beta5_beta6 <- vcov_matrix["I(exper^2)", "I(educ * exper)"]

# Compute standard error of D_hat
SE_D_hat <- sqrt(144 * var_beta5 + 16 * var_beta6 + 2 * (12) * (-4) * cov_beta5_beta6)

# Compute t-statistic
t_stat <- D_hat / SE_D_hat

# Compute p-value
p_value <- 2 * pt(-abs(t_stat), df = df.residual(model))

# Output results
cat("Estimated difference in marginal effects:", D_hat, "\n")
cat("Standard error of the difference:", SE_D_hat, "\n")
cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

# Decision rule
if (p_value < 0.05) {
  cat("Reject H0: Wendy and Jill have significantly different marginal effects of experience.\n")
} else {
  cat("Fail to reject H0: No significant difference in their marginal effects of experience.\n")
}

##(i)
# Extract coefficient estimates
beta4_hat <- coef(model)["exper"]
beta5_hat <- coef(model)["I(exper^2)"]
beta6_hat <- coef(model)["I(educ * exper)"]

# Solve for EXPER when ME = 0
EXPER_critical <- (-beta4_hat - 16 * beta6_hat) / (2 * beta5_hat)

# Extract variance and covariance from the model's variance-covariance matrix
vcov_matrix <- vcov(model)
var_beta4 <- vcov_matrix["exper", "exper"]
var_beta5 <- vcov_matrix["I(exper^2)", "I(exper^2)"]
var_beta6 <- vcov_matrix["I(educ * exper)", "I(educ * exper)"]
cov_beta4_beta6 <- vcov_matrix["exper", "I(educ * exper)"]
cov_beta4_beta5 <- vcov_matrix["exper", "I(exper^2)"]
cov_beta5_beta6 <- vcov_matrix["I(exper^2)", "I(educ * exper)"]

# Compute standard error using the delta method
SE_EXPER_critical <- (1 / abs(2 * beta5_hat)) * sqrt(
  var_beta4 + 16^2 * var_beta6 + 4 * (EXPER_critical)^2 * var_beta5 +
    2 * (16) * cov_beta4_beta6 - 4 * (EXPER_critical) * cov_beta4_beta5 - 
    8 * (16) * (EXPER_critical) * cov_beta5_beta6
)

# Compute 95% confidence interval
lower_bound <- EXPER_critical - 1.96 * SE_EXPER_critical
upper_bound <- EXPER_critical + 1.96 * SE_EXPER_critical

# Output results
cat("Jill's estimated years of experience when marginal effect becomes negative:", EXPER_critical, "\n")
cat("95% Confidence Interval: (", lower_bound, ",", upper_bound, ")\n")
