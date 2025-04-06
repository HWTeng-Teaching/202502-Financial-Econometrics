# Question 6

# a
qt(0.975, df = 60)
alpha <- 0.05
df <- 63 - 3  # n - number of parameters
tcrit <- qt(1 - alpha / 2, df)
b2 <- 3
seb2 <- sqrt(4)  # standard error = sqrt of variance (2nd diagonal entry)
t <- b2 / seb2
pval <- 2 * (1 - pt(abs(t), df))  # two-tail test
pval

# b
b <- c(2, 3, -1)
vcov_mat <- matrix(c(3, -2, 1,
                     -2, 4, 0,
                     1, 0, 3), nrow = 3, byrow = TRUE)

c_vec <- c(1, 2, 0)
g <- 5
Cb_minus_g <- sum(c_vec * b) - g
var_Cb <- t(c_vec) %*% vcov_mat %*% c_vec
t <- Cb_minus_g / sqrt(var_Cb)
t
pval <- 2 * (1 - pt(abs(t), df))
pval

# c
c_vec <- c(1, -1, 1)
g <- 4
Cb_minus_g <- sum(c_vec * b) - g
var_Cb <- t(c_vec) %*% vcov_mat %*% c_vec
t <- Cb_minus_g / sqrt(var_Cb)
t
pval <- 2 * (1 - pt(abs(t), df))
pval

# Question 31
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/commute5.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

# a
model = lm(time ~ depart + reds + trains, data = commute5)
summary(model)
vcov = vcov(model)

# b
b1 = coef(model)[1]
b2 = coef(model)[2]
b3 = coef(model)[3]
b4 = coef(model)[4]
varb1 = vcov(model)[1, 1]
varb2 = vcov(model)[2, 2]
varb3 = vcov(model)[3, 3]
varb4 = vcov(model)[4, 4]
alpha  = 0.05
df = nrow(commute5) - 4
tcr = qt(1 - alpha / 2, df)
b1 - tcr * sqrt(varb1)
b1 + tcr * sqrt(varb1)
b2 - tcr * sqrt(varb2)
b2 + tcr * sqrt(varb2)
b3 - tcr * sqrt(varb3)
b3 + tcr * sqrt(varb3)
b4 - tcr * sqrt(varb4)
b4 + tcr * sqrt(varb4)
confints = confint(model, level = 0.95)
confints

# c
qt(0.95, df)
sqrt(varb3)
(b3 - 2)/sqrt(varb3)

# d
qt(1-0.1/2, df)
sqrt(varb4)
(b4 - 3)/sqrt(varb4)

# e
sqrt(varb2)
(b2 - 1/3)/sqrt(varb2)

# f
covb4b3 = vcov(model)[4, 3]
se = sqrt(varb4 + 9*varb3 - 6*covb4b3)
(b4 - 3*b3)/se

# g & h
a <- c(1, 30, 6, 1)
# Calculate the variance of the linear combination
variance <- t(a) %*% vcov %*% a
sqrt(variance)
covb1b2 = vcov(model)[1, 2]
covb1b3 = vcov(model)[1, 3]
covb1b4 = vcov(model)[1, 4]
covb2b3 = vcov(model)[2, 3]
covb2b4 = vcov(model)[2, 4]
covb3b4 = vcov(model)[3, 4]
# Double check:
varb1 + 30^2*varb2 + 6^2*varb3 + varb4 + 2*30*covb1b2 + 2*6*covb1b3 + 2*covb1b4 + 
  2*30*6*covb2b3 + 2*30*covb2b4 + 2*6*covb3b4
(b1 + 30*b2 + 6*b3 + b4 - 45)/sqrt(variance)

# Question 33
library(ggplot2)     # For plotting the histogram
library(moments)     # For skewness and kurtosis functions
library(tidyverse)
library(tseries)

# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

# a & b
model1 = lm(log(wage) ~ educ + I(educ^2) + exper + I(exper^2) + I(educ*exper), data = cps5_small)
sm1 = summary(model1)
sm1
sm1$sigma
vcov1 = vcov(model1)
b2 = coef(model1)[2]
b3 = coef(model1)[3]
b6 = coef(model1)[6]
cps5_small = cps5_small %>% mutate(
  me_educ = b2 + 2 * b3 * educ + b6 * exper 
)

# c
# Create histogram of 'me_educ' variable
ggplot(cps5_small, aes(x = me_educ)) +
  geom_histogram(binwidth = 0.002, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Marginal Effects (me_educ)", 
       x = "Marginal Effects", y = "Frequency") +
  theme_minimal()

# Compute summary statistics
mean_me_educ <- mean(cps5_small$me_educ, na.rm = TRUE)
median_me_educ <- median(cps5_small$me_educ, na.rm = TRUE)
max_me_educ <- max(cps5_small$me_educ, na.rm = TRUE)
min_me_educ <- min(cps5_small$me_educ, na.rm = TRUE)
percentile_5 <- quantile(cps5_small$me_educ, 0.05, na.rm = TRUE)
percentile_95 <- quantile(cps5_small$me_educ, 0.95, na.rm = TRUE)
std_dev_me_educ <- sd(cps5_small$me_educ, na.rm = TRUE)
skewness_me_educ <- skewness(cps5_small$me_educ, na.rm = TRUE)
kurtosis_me_educ <- kurtosis(cps5_small$me_educ, na.rm = TRUE)
jarque_bera_test <- jarque.bera.test(cps5_small$me_educ)

# Display summary statistics
summary_stats <- data.frame(
  Mean = mean_me_educ,
  Median = median_me_educ,
  Maximum = max_me_educ,
  Minimum = min_me_educ,
  Percentile_5 = percentile_5,
  Percentile_95 = percentile_95,
  Std_Dev = std_dev_me_educ,
  Skewness = skewness_me_educ,
  Kurtosis = kurtosis_me_educ,
  Jarque_Bera_Stat = jarque_bera_test$statistic,
  Jarque_Bera_p_value = jarque_bera_test$p.value
)

print(summary_stats)

# d & e
b4 = coef(model1)[4]
b5 = coef(model1)[5]
cps5_small = cps5_small %>% mutate(
  me_exper = b4 + 2 * b5 * exper + b6 * educ 
)
# Create histogram of 'me_exper' variable
ggplot(cps5_small, aes(x = me_exper)) +
  geom_histogram(binwidth = 0.002, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Marginal Effects (me_exper)", 
       x = "Marginal Effects", y = "Frequency") +
  theme_minimal()

# Compute summary statistics
mean_me_exper <- mean(cps5_small$me_exper, na.rm = TRUE)
median_me_exper <- median(cps5_small$me_exper, na.rm = TRUE)
max_me_exper <- max(cps5_small$me_exper, na.rm = TRUE)
min_me_exper <- min(cps5_small$me_exper, na.rm = TRUE)
percentile_5_exper <- quantile(cps5_small$me_exper, 0.05, na.rm = TRUE)
percentile_95_exper <- quantile(cps5_small$me_exper, 0.95, na.rm = TRUE)
std_dev_me_exper <- sd(cps5_small$me_exper, na.rm = TRUE)
skewness_me_exper <- skewness(cps5_small$me_exper, na.rm = TRUE)
kurtosis_me_exper <- kurtosis(cps5_small$me_exper, na.rm = TRUE)
jarque_bera_test_exper <- jarque.bera.test(cps5_small$me_exper)

# Display summary statistics
summary_stats_exper <- data.frame(
  Mean = mean_me_exper,
  Median = median_me_exper,
  Maximum = max_me_exper,
  Minimum = min_me_exper,
  Percentile_5 = percentile_5_exper,
  Percentile_95 = percentile_95_exper,
  Std_Dev = std_dev_me_exper,
  Skewness = skewness_me_exper,
  Kurtosis = kurtosis_me_exper,
  Jarque_Bera_Stat = jarque_bera_test_exper$statistic,
  Jarque_Bera_p_value = jarque_bera_test_exper$p.value
)

print(summary_stats_exper)

# f
qt(0.05, 1194)
a1 = c(0, -1, -33, 10, 260, 152) 
t1 = -b2-33*b3+10*b4+260*b5+152*b6
# Calculate the variance of the linear combination
variance <- t(a1) %*% vcov1 %*% a1
sqrt(variance)
t/sqrt(variance)

# g
a2 = c(0, -1, -33, 10, 420, 144)
variance <- t(a2) %*% vcov1 %*% a2
sqrt(variance)
t2 = -b2-33*b3+10*b4+420*b5+144*b6
t2
t2/sqrt(variance)

# h
tcr = qt(0.975, 1194)
h1 = 12*b5 - 4*b6
h2 =sqrt(12^2*vcov1[5, 5] + 4^2*vcov1[6, 6] - 2*12*4*vcov1[5, 6])
h1/h2

# i
b4 + 22*b5 + 16*b6
b5
year = -(b4 + 22*b5 + 16*b6)/(2*b5)
g4 = -0.5/b5
g5 = (b4 + 16*b6)*0.5/(b5^2)
g6 = -8/b5
g4
g5
g6
g = c(0, 0, 0, g4, g5, g6)
# Calculate the variance of the linear combination
vcov1
variance <- t(g) %*% vcov1 %*% g
sqrt(variance)
year - tcr*sqrt(variance)
year + tcr*sqrt(variance)
