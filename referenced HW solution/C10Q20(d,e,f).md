![image](https://github.com/user-attachments/assets/99e7da4f-bc91-4c8f-a099-91f90528267f)
## 
![image](https://github.com/user-attachments/assets/9b577874-fdb7-4109-bd24-5850be2b4e8a)

IV/2SLS Estimation Using RANK as an Instrument. 
We estimate the CAPM model using two-stage least squares (2SLS), where the market excess return (excess_mkt) is treated as a potentially endogenous regressor. The instrument used is RANK, which is defined as the rank ordering of excess_mkt from smallest to largest.
```r
# Load the AER package for instrumental variable regression
library(AER)

# Perform 2SLS estimation using ivreg()
iv_model <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm)

# Display the regression results
summary(iv_model)

