![image](https://github.com/user-attachments/assets/99e7da4f-bc91-4c8f-a099-91f90528267f)
## 
![image](https://github.com/user-attachments/assets/9b577874-fdb7-4109-bd24-5850be2b4e8a)

## (d) IV/2SLS Estimation Using `RANK` as an Instrument

In this section, we estimate the CAPM model using two-stage least squares (2SLS), treating the market excess return as a potentially endogenous regressor. The instrument used is `RANK`, which is constructed by ranking the values of the market excess return from smallest to largest.

---

### Step 1: Construct the Instrument `RANK`

```r
# Load the AER package for instrumental variable regression
library(AER)

# from smallest to largest. Each observation receives a rank between 1 and 180.
capm$RANK <- rank(capm$excess_mkt, ties.method = "first")

# Perform 2SLS estimation using ivreg()
iv_model <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm)

# Display the regression results
summary(iv_model)

