library(POE5Rdata)
data(cocaine)
##5.23(a)
## I expect that all of them are positive since if the quality and quantity of cocaine increases, it should also increase the price of the cocaine; similarly, I think the time trend is positive because there might be a scarce in the resources of cocaine.

##(b)
library(ggplot2)

model_cocaine <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model_cocaine)
## The signs of quantity and time trend are different from my expectation. 

##(c)
summary(model_cocaine)$r.squared

##(d)
library(car)
## Ho: beta2>0, Ha:beta2<=0
# Extract coefficient estimate and standard error for QUANT
beta_2 <- summary(model_cocaine)$coefficients["quant", "Estimate"]
se_beta_2 <- summary(model_cocaine)$coefficients["quant", "Std. Error"]

t_stat <- beta_2 / se_beta_2

p_value <- pt(t_stat, df = df.residual(model_cocaine))

cat("t-statistic:", t_stat, "\n")
cat("One-tailed p-value:", p_value, "\n")

alpha <- 0.05
if (p_value < alpha) {
  cat("Reject H0: Larger quantity is associated with lower price.\n")
} else {
  cat("Fail to reject H0: No strong evidence that larger quantity lowers price.\n")
}

##(e)
beta_3 <- summary(model_cocaine)$coefficients["qual", "Estimate"]
se_beta_3 <- summary(model_cocaine)$coefficients["qual", "Std. Error"]

t_stat <- beta_3 / se_beta_3

p_value <- 1 - pt(t_stat, df = df.residual(model_cocaine))

cat("t-statistic:", t_stat, "\n")
cat("One-tailed p-value:", p_value, "\n")

alpha <- 0.05
if (p_value < alpha) {
  cat("Reject H0: A premium is paid for higher-quality cocaine.\n")
} else {
  cat("Fail to reject H0: No strong evidence that quality affects price.\n")
}

##(f)
annual_change <- summary(model_cocaine)$coefficients["trend", "Estimate"]
cat("Estimated average annual change in price:", annual_change, "\n")
## This might be due to a law enforcement.
