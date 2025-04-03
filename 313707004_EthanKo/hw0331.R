library(POE5Rdata)
#5.31(a)
data(commute5)

model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)

#(b)
confint(model, level = 0.95)
## This is not the precise estimates for the model, in fact the 95%-interval contains the true parameters.

##(c)
beta3_hat <- coef(model)["reds"]
se_beta3 <- summary(model)$coefficients["reds", "Std. Error"]

t_stat <- (beta3_hat - 2) / se_beta3

p_value <- pt(t_stat, df = df.residual(model))

cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Reject H0: The expected delay from a red light is significantly less than 2 minutes.\n")
} else {
  cat("Fail to reject H0: The evidence does not strongly suggest the delay is less than 2 minutes.\n")
}

##(d)
beta4_hat <- coef(model)["trains"]
se_beta4 <- summary(model)$coefficients["trains", "Std. Error"]

t_stat <- (beta4_hat - 3) / se_beta4

p_value <- 2 * pt(-abs(t_stat), df = df.residual(model))

cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

if (p_value < 0.10) {
  cat("Reject H0: The expected delay from a train is significantly different from 3 minutes.\n")
} else {
  cat("Fail to reject H0: The evidence does not strongly suggest the delay is different from 3 minutes.\n")
}

##(e)
beta2_hat <- coef(model)["depart"]
se_beta2 <- summary(model)$coefficients["depart", "Std. Error"]

t_stat <- (beta2_hat - 0.3333) / se_beta2

p_value <- pt(t_stat, df = df.residual(model))

cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Reject H0: Bill's expected increase in trip time is significantly less than 10 minutes.\n")
} else {
  cat("Fail to reject H0: The evidence does not strongly suggest the trip increase is less than 10 minutes.\n")
}

##(f)
beta3_hat <- coef(model)["reds"]
beta4_hat <- coef(model)["trains"]
se_beta3 <- summary(model)$coefficients["reds", "Std. Error"]
se_beta4 <- summary(model)$coefficients["trains", "Std. Error"]

cov_beta3_beta4 <- vcov(model)["reds", "trains"]

se_diff <- sqrt(se_beta4^2 + 9 * se_beta3^2 + 6 * cov_beta3_beta4)

t_stat <- (beta4_hat - 3 * beta3_hat) / se_diff

p_value <- pt(t_stat, df = df.residual(model))

cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Reject H0: The expected delay from a train is significantly less than three times the delay from a red light.\n")
} else {
  cat("Fail to reject H0: The evidence does not strongly suggest the train delay is less than three times the red light delay.\n")
}

##(g)
beta1_hat <- coef(model)["(Intercept)"]
beta2_hat <- coef(model)["depart"]
beta3_hat <- coef(model)["reds"]
beta4_hat <- coef(model)["trains"]

se_beta1 <- summary(model)$coefficients["(Intercept)", "Std. Error"]
se_beta2 <- summary(model)$coefficients["depart", "Std. Error"]
se_beta3 <- summary(model)$coefficients["reds", "Std. Error"]
se_beta4 <- summary(model)$coefficients["trains", "Std. Error"]

DEPART <- 30  # 7:00 AM
REDS <- 6
TRAINS <- 1

predicted_time <- beta1_hat + beta2_hat * DEPART + beta3_hat * REDS + beta4_hat * TRAINS

cov_matrix <- vcov(model)
var_pred <- cov_matrix["(Intercept)", "(Intercept)"] +
  (DEPART)^2 * cov_matrix["depart", "depart"] +
  (REDS)^2 * cov_matrix["reds", "reds"] +
  (TRAINS)^2 * cov_matrix["trains", "trains"] +
  2 * (DEPART * cov_matrix["(Intercept)", "depart"] +
         REDS * cov_matrix["(Intercept)", "reds"] +
         TRAINS * cov_matrix["(Intercept)", "trains"]) +
  2 * (DEPART * REDS * cov_matrix["depart", "reds"] +
         DEPART * TRAINS * cov_matrix["depart", "trains"] +
         REDS * TRAINS * cov_matrix["reds", "trains"])

se_pred <- sqrt(var_pred)

t_stat <- (predicted_time - 45) / se_pred

p_value <- 1 - pt(t_stat, df = df.residual(model))

cat("Predicted travel time:", predicted_time, "\n")
cat("Standard error of prediction:", se_pred, "\n")
cat("t-statistic:", t_stat, "\n")
cat("p-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Reject H0: Bill is likely to arrive later than 7:45 AM.\n")
} else {
  cat("Fail to reject H0: The evidence does not strongly suggest Bill will arrive later than 7:45 AM.\n")
}

##(h)
#The original hypothesis setup is correct because it is framed around the primary concern: ensuring Bill is not late.

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

