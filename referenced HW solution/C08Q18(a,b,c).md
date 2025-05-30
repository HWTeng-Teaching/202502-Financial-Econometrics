***學號：313652013     應數所碩一 : 鄭凱鴻***

![image](https://github.com/user-attachments/assets/1a79a574-9e6c-4304-92d7-a5222eb4822c)


## (a) 
**Hypothesis:**

- $H_0$: $\sigma^2_{\text{male}} = \sigma^2_{\text{female}}$ (homoskedasticity)
- $H_1$: $\sigma^2_{\text{male}} \neq \sigma^2_{\text{female}}$ (heteroskedasticity)

**Through R programing, we can get the result:**

**Test Statistic (F) :** 
$\hat{\sigma}^2_{\text{male}}/ \hat{\sigma}^2_{\text{female}}=$
1.05076

**Critical Region (5% significance level):**  
F < 0.9451 or F > 1.0581

Since the Test Statistic does not fall in the Critical Region, we Fail to reject the null hypothesis.

**Conclusion:**    
There is no significant difference in wage variance between males and females.


## (b)
### Case 1: Using only `metro`, `female`, and `black` as candidate variables related to the heteroskedasticity

**Hypothesis:**
- $H_0$: Error variance is constant and unrelated to metro, female, or black.  
- $H_1$: Error variance depends on at least one of metro, female, or black.

**Through R programming, we obtain the following results:**

- **Test Statistic (NR²) :** 23.5568
- **1% critical value** (χ² with 4-1 degrees of freedom): 11.34487
- **Degrees of freedom:** 3  
- **p-value:** 3.0909×10⁻⁵

**Conclusion:**  
Since the test Statistic is bigger than critical value, we reject the null hypothesis of homoskedasticity at the 1% level.  
This indicates that heteroskedasticity is present.

---

### Case 2: Using all explanatory variables as candidate variables related to the heteroskedasticity

**Hypothesis:**
- $H_0$: Error variance is constant and unrelated to all regressors.  
- $H_1$: Error variance depends on at least one regressor.

**Through R programming, we obtain the following results:**

- **Test Statistic (NR²) :** 109.4243
- **1% critical value** (χ² with 10-1 degrees of freedom): 21.66599
- **Degrees of freedom:** 9  
- **p-value:** 1.925849×10⁻¹⁹

**Conclusion:**  
Again, the test Statistic is far bigger than 1% critical value. We strongly reject the null hypothesis, concluding that the model exhibits significant heteroskedasticity.

---

### Overall Interpretation

Although the Goldfeld–Quandt test in question (a) showed no evidence of heteroskedasticity by gender,  
the NR² test results in question (b) reveal that heteroskedasticity is present in the model and may be related to other variables such as metro or black.  

## (c) 

**Hypothesis:**
- $H_0$: Error variance is constant and unrelated to all regressors.  
- $H_1$: Error variance depends on at least one regressor.
  
**Through R programming, we obtain the following results:**

- **Test Statistic (NR²):** 165.46
- **Degrees of freedom:** 41
- **p-value:** 2.2 × 10⁻¹⁶
- **5% critical value** (χ² with 41 degrees of freedom): 56.94239

**Conclusion:**  
Since the test statistic exceeds the critical value, we reject the null hypothesis of homoskedasticity.
There is strong evidence of heteroskedasticity in the model.
