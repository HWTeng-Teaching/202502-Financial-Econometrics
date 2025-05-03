![image](https://github.com/user-attachments/assets/99e7da4f-bc91-4c8f-a099-91f90528267f)
## 
![image](https://github.com/user-attachments/assets/9b577874-fdb7-4109-bd24-5850be2b4e8a)

## (d) IV/2SLS Estimation Using `RANK` as an Instrument

In this section, we estimate the CAPM model using two-stage least squares (2SLS), treating the market excess return as a potentially endogenous regressor. The instrument used is `RANK`, which is constructed by ranking the values of the market excess return from smallest to largest.

---

### Step 1: Construct the Instrument `RANK`

```r
# Create the instrumental variable RANK by ranking the market excess returns
# from smallest to largest. Ties are assigned based on order of appearance.
capm$RANK <- rank(capm$excess_mkt, ties.method = "first")
```

---

### Step 2: First‐Stage Regression

We regress `excess_mkt` on `RANK` to obtain fitted values.

excess_mkt<sub>t</sub> = π<sub>0</sub> + π<sub>1</sub> · RANK<sub>t</sub> + v<sub>t</sub>

```r
first_stage <- lm(excess_mkt ~ RANK, data = capm)
summary(first_stage)
```

**Results:**
- R<sup>2</sup> = 0.913  
- F-statistic = 1858  
- `RANK` is **highly significant** (p < 0.001)  

> This confirms that `RANK` is a **strong instrument**.

---

### Step 3: 2SLS Estimation using `ivreg()`

We estimate the CAPM equation using `RANK` as the instrument for `excess_mkt`.

excess_msft<sub>t</sub> = α + β · excess_mkt<sub>t</sub> + ε<sub>t</sub>


```r
library(AER)

iv_model <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm)
summary(iv_model)
```

---

### Estimation Results

| Method   | Beta (Market Coefficient) | Std. Error | t Value | Significance |
|----------|---------------------------:|-----------:|--------:|--------------|
| **OLS**  | 1.2018                     | 0.1222     | 9.839   | ***          |
| **2SLS** | 1.2783                     | 0.1280     | 9.986   | ***          |

---

### Conclusion

The 2SLS estimate of β is **greater** than the OLS estimate, consistent with theoretical expectations under **measurement error**:

- OLS estimates are biased **toward zero** when regressors are measured with error.  
- The strong performance of the instrument `RANK` ensures reliable identification.

---

## 
![image](https://github.com/user-attachments/assets/44fb74dd-8c48-4fbb-bac7-6db36a39facd)

## (e) First‐Stage Regression with `RANK` and `POS` as Instruments

In this section, we extend the first‐stage regression to include both `RANK` and `POS` as instruments for the potentially endogenous regressor `excess_mkt`.

---

### Step 1: Construct the Instrument `POS`

```r
# Create the binary instrument POS: 1 if market excess return is positive, 0 otherwise
capm$POS <- ifelse(capm$excess_mkt > 0, 1, 0)
```

---

### Step 2: First‐Stage Regression

We regress `excess_mkt` on `RANK` and `POS`:


excess_mkt<sub>t</sub> = π<sub>0</sub> + π<sub>1</sub> · RANK<sub>t</sub> + π<sub>2</sub> · POS<sub>t</sub> + v<sub>t</sub>


```r
first_stage2 <- lm(excess_mkt ~ RANK + POS, data = capm)
summary(first_stage2)
```

**Results:**
- R<sup>2</sup> = 0.9149  
- F-statistic: 951.3 on 2 and 177 DF (p < 0.001)  
- `RANK` is highly significant (p < 0.001)  
- `POS` is significant at 5% level (p = 0.0291)  

> This confirms that `RANK` and `POS` are **jointly strong instruments**.

---

### Conclusion

The first‐stage regression shows that the two instruments explain over 91% of the variation in `excess_mkt` and pass the joint‐significance test (F ≫ 10). Therefore, `RANK` and `POS` form an adequately strong IV set for the subsequent 2SLS estimation.  

---

## 
![image](https://github.com/user-attachments/assets/e03717d9-86b5-45c9-83d0-b17783b2eab4)

## (f) Hausman Test for Endogeneity

In this section, we carry out the Hausman test to determine whether the market excess return (`excess_mkt`) is endogenous, using the residuals from the first‐stage regression in part (e).

---

### Step 1: Extract First‐Stage Residuals

```r
# Obtain residuals from the first-stage regression (instruments: RANK + POS)
capm$vhat2 <- resid(first_stage2)
```

---

### Step 2: Augmented Regression

We augment the original CAPM model with the first‐stage residuals:


excess_msft<sub>t</sub> = α + β · excess_mkt<sub>t</sub> + δ · vhat2<sub>t</sub> + ε<sub>t</sub>


```r
hausman_model <- lm(excess_msft ~ excess_mkt + vhat2, data = capm)
summary(hausman_model)
```

**Results:**
- Coefficient of `vhat2`: **−0.9549**  
- Std. Error: 0.4331  
- t value: −2.205  
- p value: **0.0287**

---

### Conclusion

The null hypothesis H<sub>0</sub>: &delta; = 0 (market return is exogenous) is tested at the 1% significance level.

- **p value = 0.0287 ≥ 0.01**  
- We **do not reject** H<sub>0</sub> at the 1% level.

> **Therefore, we conclude that the market excess return can be treated as exogenous at the 1% significance level.**

