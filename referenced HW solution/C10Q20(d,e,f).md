```r
# Load the AER package for instrumental variable regression
library(AER)

# Perform 2SLS estimation using ivreg()
iv_model <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm)

# Display the regression results
summary(iv_model)

