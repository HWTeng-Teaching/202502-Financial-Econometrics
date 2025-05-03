![image](https://github.com/user-attachments/assets/99e7da4f-bc91-4c8f-a099-91f90528267f)
## 
![image](https://github.com/user-attachments/assets/9b577874-fdb7-4109-bd24-5850be2b4e8a)

## 📌 (d) IV/2SLS Estimation Using `RANK` as an Instrument

In this section, we estimate the CAPM model using two-stage least squares (2SLS), treating the market excess return as a potentially endogenous regressor. The instrument used is `RANK`, which is constructed by ranking the values of the market excess return from smallest to largest.

---

### ✅ Step 1: Construct the Instrument `RANK`

```r
# Create the instrumental variable RANK by ranking the market excess returns
# from smallest to largest. Ties are assigned based on order of appearance.
capm$RANK <- rank(capm$excess_mkt, ties.method = "first")
```

---

### ✅ Step 2: First‐Stage Regression

We regress `excess_mkt` on `RANK` to obtain fitted values.

excess_mkt<sub>t</sub> = π<sub>0</sub> + π<sub>1</sub> · RANK<sub>t</sub> + v<sub>t</sub>

```r
first_stage <- lm(excess_mkt ~ RANK, data = capm)
summary(first_stage)
```

**Result highlights:**
- R<sup>2</sup> = 0.913  
- F-statistic = 1858  
- `RANK` is **highly significant** (p < 0.001)  

> ✅ This confirms that `RANK` is a **strong instrument**.

---

### ✅ Step 3: 2SLS Estimation using `ivreg()`

We estimate the CAPM equation using `RANK` as the instrument for `excess_mkt`.

```
excess_msft<sub>t</sub> = α + β · excess_mkt<sub>t</sub> + ε<sub>t</sub>
```

```r
library(AER)

iv_model <- ivreg(excess_msft ~ excess_mkt | RANK, data = capm)
summary(iv_model)
```

---

### 📊 Estimation Results

| Method   | Beta (Market Coefficient) | Std. Error | t Value | Significance |
|----------|---------------------------:|-----------:|--------:|--------------|
| **OLS**  | 1.2018                     | 0.1222     | 9.839   | ***          |
| **2SLS** | 1.2783                     | 0.1280     | 9.986   | ***          |

---

### ✅ Conclusion

The 2SLS estimate of β is **greater** than the OLS estimate, consistent with theoretical expectations under **measurement error**:

- OLS estimates are biased **toward zero** when regressors are measured with error.  
- IV estimation corrects for this attenuation bias.  
- The strong performance of the instrument `RANK` ensures reliable identification.

> **Yes**, the IV estimate agrees with expectations. It is consistent, significant, and slightly larger than the OLS estimate, validating the usefulness of `RANK` as an instrument.
