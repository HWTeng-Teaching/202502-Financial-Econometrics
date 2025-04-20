# Find the critical t-value for a 95% confidence level
qt(0.975, df = 60)

# Define significance level and degrees of freedom
alpha <- 0.05
df <- 63 - 3  # Total sample size minus number of estimated parameters

# Compute critical value from the t-distribution
tcrit <- qt(1 - alpha / 2, df)

# Specify coefficient and its standard error
b2 <- 3
seb2 <- sqrt(4)  # Standard error from the square root of variance

# Calculate t-statistic
t <- b2 / seb2

# Compute two-sided p-value
pval <- 2 * (1 - pt(abs(t), df))
pval
# Coefficients and covariance matrix
b <- c(2, 3, -1)
vcov_mat <- matrix(c(3, -2, 1,
                     -2, 4, 0,
                      1, 0, 3), nrow = 3, byrow = TRUE)

# Linear restriction
c_vec <- c(1, 2, 0)
g <- 5

# Compute the difference between c'b and g
Cb_minus_g <- sum(c_vec * b) - g

# Calculate variance of c'b
var_Cb <- t(c_vec) %*% vcov_mat %*% c_vec

# t-statistic and p-value
t <- Cb_minus_g / sqrt(var_Cb)
t
pval <- 2 * (1 - pt(abs(t), df))
pval
# New restriction
c_vec <- c(1, -1, 1)
g <- 4

# Compute deviation and variance
Cb_minus_g <- sum(c_vec * b) - g
var_Cb <- t(c_vec) %*% vcov_mat %*% c_vec

# Calculate t-statistic and p-value
t <- Cb_minus_g / sqrt(var_Cb)
t
pval <- 2 * (1 - pt(abs(t), df))
pval
# Load online dataset
con <- url("http://www.principlesofeconometrics.com/poe5/data/rdata/commute5.rdata", "rb")
load(con)
close(con)

# Estimate linear model
model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)
vcov <- vcov(model)
# Extract coefficients and variances
b1 <- coef(model)[1]; varb1 <- vcov[1,1]
b2 <- coef(model)[2]; varb2 <- vcov[2,2]
b3 <- coef(model)[3]; varb3 <- vcov[3,3]
b4 <- coef(model)[4]; varb4 <- vcov[4,4]

# Set alpha and degrees of freedom
alpha <- 0.05
df <- nrow(commute5) - 4
tcr <- qt(1 - alpha / 2, df)

# Compute confidence intervals manually
b1 ± tcr * sqrt(varb1)
b2 ± tcr * sqrt(varb2)
b3 ± tcr * sqrt(varb3)
b4 ± tcr * sqrt(varb4)

# Use built-in function to verify
confint(model, level = 0.95)
qt(0.95, df)
(b3 - 2) / sqrt(varb3)

qt(1 - 0.1 / 2, df)
(b4 - 3) / sqrt(varb4)

(b2 - 1/3) / sqrt(varb2)
covb4b3 <- vcov[4,3]
se <- sqrt(varb4 + 9*varb3 - 6*covb4b3)
(b4 - 3*b3) / se
covb4b3 <- vcov[4,3]
se <- sqrt(varb4 + 9*varb3 - 6*covb4b3)
(b4 - 3*b3) / se
a <- c(1, 30, 6, 1)

# Compute variance of linear combination
variance <- t(a) %*% vcov %*% a
sqrt(variance)

# Double check manually
# ...
(b1 + 30*b2 + 6*b3 + b4 - 45) / sqrt(variance)
# Load dataset
con <- url("http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata", "rb")
load(con)
close(con)

# Run nonlinear regression
model1 <- lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ*exper), data = cps5_small)
summary(model1)

# Extract coefficients
b2 <- coef(model1)[2]
b3 <- coef(model1)[3]
b6 <- coef(model1)[6]

# Compute marginal effect of education
cps5_small <- cps5_small %>%
  mutate(me_educ = b2 + 2*b3*educ + b6*exper)
# Plot histogram
ggplot(cps5_small, aes(x = me_educ)) +
  geom_histogram(...)

# Summary stats
summary_stats <- data.frame(
  Mean = mean(...), Median = median(...), ...
)
b4 <- coef(model1)[4]
b5 <- coef(model1)[5]

cps5_small <- cps5_small %>%
  mutate(me_exper = b4 + 2*b5*exper + b6*educ)

# Histogram and summary stats like above
qt(0.05, 1194)
a1 <- c(0, -1, -33, 10, 260, 152)
t1 <- -b2 - 33*b3 + 10*b4 + 260*b5 + 152*b6

variance <- t(a1) %*% vcov1 %*% a1
t1 / sqrt(variance)
a2 <- c(0, -1, -33, 10, 420, 144)
t2 <- -b2 - 33*b3 + 10*b4 + 420*b5 + 144*b6
t2 / sqrt(t(a2) %*% vcov1 %*% a2)
a2 <- c(0, -1, -33, 10, 420, 144)
t2 <- -b2 - 33*b3 + 10*b4 + 420*b5 + 144*b6
t2 / sqrt(t(a2) %*% vcov1 %*% a2)
h1 <- 12*b5 - 4*b6
h2 <- sqrt(12^2*vcov1[5,5] + 4^2*vcov1[6,6] - 2*12*4*vcov1[5,6])
h1 / h2
# Compute turning point in years of experience
turning_point <- -(b4 + 22*b5 + 16*b6)/(2*b5)

# Use delta method to compute standard error of turning point
g <- c(0, 0, 0, -0.5/b5, (b4 + 16*b6)*0.5/(b5^2), -8/b5)
variance <- t(g) %*% vcov1 %*% g
se <- sqrt(variance)

# Confidence interval for turning point
turning_point ± tcr * se









