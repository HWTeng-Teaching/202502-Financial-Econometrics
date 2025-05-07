**學號：313707006    財金碩一 : 王彩寧**
___
### 10.20 The CAPM [see Exercises 10.14 and 2.16] says that the risk premium on security j is related to the risk premium on the market portfolio. That is rj − r𝑓 = αj + βj(rm − r𝑓) where rj and rf are the returns to security j and the risk-free rate, respectively, rm is the return on the market portfolio, and βj is the jth security’s “beta” value. We measure the market portfolio using the Standard & Poor’s value weighted index, and the risk-free rate by the 30-day LIBOR monthly rate of return. As noted in Exercise 10.14, if the market return is measured with error, then we face an errors-in-variables, or measurement error, problem.

### (a) Use the observations on Microsoft in the data file capm5 to estimate the CAPM model using OLS. How would you classify the Microsoft stock over this period? Risky or relatively safe, relative to the market portfolio?
```r
# R code
capm5$msft_excess <- capm5$msft - capm5$riskfree
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm_model <- lm(msft_excess ~ mkt_excess, data = capm5)
summary(capm_model)
```
![1](https://github.com/user-attachments/assets/fd675bd4-4bec-46be-842e-592470282ee6)

Microsoft beta is 1.2018.
Microsoft stock is relatively risky compared to the market portfolio. 


### (b) It has been suggested that it is possible to construct an IV by ranking the values of the explanatory variable and using the rank as the IV, that is, we sort (rm − r𝑓) from smallest to largest, and assign the values RANK = 1, 2, . . . . , 180. Does this variable potentially satisfy the conditions IV1–IV3? Create RANK and obtain the first-stage regression results. Is the coefficient of RANK very significant? What is the R2 of the first-stage regression? Can RANK be regarded as a strong IV?
```r
# R code
capm5$RANK <- rank(capm5$mkt_excess)
first_stage <- lm(mkt_excess ~ RANK, data = capm5)
summary(first_stage)
linearHypothesis(first_stage, c("RANK=0"))
```
![2](https://github.com/user-attachments/assets/20692682-e56d-4d61-bdec-d345b9a124c1)
![5](https://github.com/user-attachments/assets/4468f340-760a-4ad4-9183-6134c13df191)


The variable RANK does not directly cause Microsoft’s return, so it meets condition.
R^2 = 0.9126.
The t-value is 43.10, and the F-statistic is 1857.61.
This means that RANK is a very strong instrument.

### (c) Compute the first-stage residuals, ̂v, and add them to the CAPM model. Estimate the resulting augmented equation by OLS and test the significance of ̂v at the 1% level of significance. Can we conclude that the market return is exogenous?
```r
# R code
capm5$v_hat <- resid(first_stage)
second_stage <- lm(msft_excess ~ mkt_excess + v_hat, data = capm5)
summary(second_stage)
```
![3](https://github.com/user-attachments/assets/2b5e2d02-a7fc-4bec-bf4a-803fc2a35bd5)

The t-statistic on v ̂ is −2.04 with a p = 0.043.
It is not significant at the 1% level, but at the 5% level. 
At the 1% level, we cannot reject the null hypothesis that the market return is exogenous.

