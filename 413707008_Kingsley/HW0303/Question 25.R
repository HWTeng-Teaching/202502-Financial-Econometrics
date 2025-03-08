# Question 25
# Define the URL
url <- "http://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"
# Open a connection to the URL
con <- url(url, "rb")  # "rb" = read binary mode
# Load the RData file directly from the web
load(con)
# Close the connection
close(con)

#install.packages(tidyverse)
library(tidyverse)

# a.
hist(cex5_small$foodaway)
summary(cex5_small$foodaway)

# b.
cex5_small_advanced = cex5_small %>% filter(advanced == 1)
summary(cex5_small_advanced$foodaway)
cex5_small_college = cex5_small %>% filter(college == 1)
summary(cex5_small_college$foodaway)
cex5_small_none = cex5_small %>% filter(advanced == 0 & college == 0)
summary(cex5_small_none$foodaway)

# c.
cex5_small_ln = cex5_small %>% mutate(
  ln_foodaway = log(foodaway)
) %>% filter(is.finite(ln_foodaway))
hist(cex5_small_ln$ln_foodaway)
summary(cex5_small_ln$ln_foodaway)
length(cex5_small$foodaway)
length(cex5_small_ln$ln_foodaway)

# d.
model1 = lm(ln_foodaway ~ income, data = cex5_small_ln)
summary(model1)

# e.
# 1) Create the scatter plot
plot(
  cex5_small_ln$income, 
  cex5_small_ln$ln_foodaway,
  xlab = "Household Income",
  ylab = "ln(FOODAWAY)",
  main = "ln(FOODAWAY) vs. INCOME"
)

# 2) Add the fitted regression line
abline(model1, col = "red", lwd = 2)

# f.
# 1) Calculate least squares residuals
residuals_model1 <- resid(model1)

# 2) Plot residuals vs. INCOME
plot(
  cex5_small_ln$income, residuals_model1,
  xlab = "Household Income",
  ylab = "Residuals from lm(ln_foodaway ~ income)",
  main = "Residuals vs. INCOME"
)
abline(h = 0, col = "red", lty = 2, lwd = 2)  # reference line at 0
