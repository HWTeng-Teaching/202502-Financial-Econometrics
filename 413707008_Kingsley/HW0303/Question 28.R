#install.packages(tidyverse)
#install.packages(stargazer)
library(tidyverse)
library(stargazer)

# Question 28
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cps5_small.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

# a.
hist(cps5_small$wage)
summary(cps5_small$wage)
hist(cps5_small$educ)
summary(cps5_small$educ)

# b.
model1 = lm(wage ~ educ, data = cps5_small)
summary(model1)

# c.
# Extract residuals
resid_model1 <- resid(model1)

# Plot residuals vs. EDUC
plot(
  cps5_small$educ, resid_model1,
  xlab = "Years of Education (EDUC)",
  ylab = "Residuals (wage - fitted)",
  main = "Residuals vs. EDUC"
)
abline(h = 0, col = "red", lty = 2)  # horizontal reference line

# d.
female = cps5_small %>% filter(female == 1)
male = cps5_small %>% filter(female == 0)
black = cps5_small %>% filter(black == 1)
white = cps5_small %>% filter(black == 0)

model_female <- lm(wage ~ educ, data = female)
model_male   <- lm(wage ~ educ, data = male)
model_black  <- lm(wage ~ educ, data = black)
model_white  <- lm(wage ~ educ, data = white)

stargazer(
  model_female, model_male, model_black, model_white,
  type = "text",                 # "text" prints to console. "html" or "latex" are also possible
  title = "Comparison of Regressions by Gender and Race",
  column.labels = c("Female","Male","Black","White"),
  dep.var.labels = "Wage",
  covariate.labels = c("Education"),
  digits = 3
)

# e.
# Fit the quadratic model: WAGE ~ EDUC^2
model_quad <- lm(wage ~ I(educ^2), data = cps5_small)
summary(model_quad)

# Extract the coefficient alpha2
a2 <- coef(model_quad)["I(educ^2)"]

# Marginal effect at EDUC = 12
ME_12 <- 2 * a2 * 12
ME_12

# Marginal effect at EDUC = 16
ME_16 <- 2 * a2 * 16
ME_16

# Marginal effect (linear)
b2 <- coef(model1)["educ"]
b2

# f.
# (1) Generate a sequence of educ values covering the observed range
educ_seq <- seq(min(cps5_small$educ), max(cps5_small$educ), length.out = 200)

# (2) Predict wage from both models along this sequence
pred_lin <- predict(model1, newdata = data.frame(educ = educ_seq))
pred_quad <- predict(model_quad, newdata = data.frame(educ = educ_seq))

# (3) Plot the raw data
plot(
  cps5_small$educ, cps5_small$wage,
  xlab = "Years of Education (EDUC)",
  ylab = "WAGE",
  main = "Comparing Linear vs. Quadratic Fits"
)

# (4) Add the linear fit (red line)
lines(educ_seq, pred_lin, col = "red", lwd = 2)

# (5) Add the quadratic fit (blue curve)
lines(educ_seq, pred_quad, col = "blue", lwd = 2)

# (6) Add a legend
legend(
  "topleft", 
  legend = c("Linear Fit", "Quadratic Fit"),
  col = c("red", "blue"), lty = 1, lwd = 2
)
