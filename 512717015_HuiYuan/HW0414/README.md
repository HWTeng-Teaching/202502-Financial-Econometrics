# 8.6
![image](https://github.com/user-attachments/assets/72c234aa-57e3-4973-8b9a-357f220be2c5)

## a
Test whether the error variances are equal for males and females
**Data:**

- Male sample size (n<sub>M</sub> = 577), df<sub>M</sub> = 577 − 4 = 573  
- SSE<sub>M</sub> = 97161.9174  
- Female estimate: σ̂<sub>F</sub> = 12.024 → σ̂²<sub>F</sub> = 144.5766

**Hypotheses:**

- H₀: σ̂²<sub>M</sub> = σ̂²<sub>F</sub>  
- H₁: σ̂²<sub>M</sub> ≠ σ̂²<sub>F</sub>

**Estimate variance for males:**

σ̂²<sub>M</sub> = 97161.9174 / 573 ≈ 169.567

**Test statistic:**

GQ = 169.567 / 144.5766 ≈ **1.173**

**Critical values:**

- F<sub>573,419,0.025</sub> = 0.8377  
- F<sub>573,419,0.975</sub> = 1.1968

**Conclusion:**  
Since 0.8377 < 1.173 < 1.1968, we **fail to reject H₀**.  
→ No sufficient evidence to indicate that error variances differ between genders.

## b
Test whether the error variances differ between married and single individuals

**Model:**  
WAGE<sub>i</sub> = β₁ + β₂ EDU<sub>i</sub> + β₃ EXPER<sub>i</sub> + β₄ METRO<sub>i</sub> + β₅ FEMALE<sub>i</sub> + e<sub>i</sub>

**Data:**

- Single: n = 400, df = 395, SSE = 56231.0382  
- Married: n = 600, df = 595, SSE = 100703.0471

**Hypotheses:**

- H₀: σ²<sub>MARRIED</sub> = σ²<sub>SINGLE</sub>  
- H₁: σ²<sub>MARRIED</sub> > σ²<sub>SINGLE</sub>

**Calculate variances:**

- σ²<sub>SINGLE</sub> = 56231.0382 / 395 ≈ 142.357  
- σ²<sub>MARRIED</sub> = 100703.0471 / 595 ≈ 169.248

**Test statistic:**

GQ = 169.248 / 142.357 ≈ **1.1889**

**Critical value (one-tailed):**  
F<sub>0.95,595,395</sub> ≈ **1.1647**

**Conclusion:**  
1.1889 > 1.1647 → **reject H₀**  
→ Married individuals have significantly greater error variance.

## c
NR² Test for Heteroskedasticity

**Hypotheses:**

- H₀: α₂ = α₃ = α₄ = α₅ = 0 (Homoskedasticity)  
- H₁: Not all α<sub>i</sub> = 0 (Heteroskedasticity)

**Test statistic:**  
NR² = 59.03  
χ²<sub>0.95,4</sub> = 9.49

**Conclusion:**  
59.03 > 9.49 → **reject H₀**  
→ Heteroskedasticity exists in the model.

This aligns with result in (b), confirming variance difference between marital statuses.   

## d
White Test

**Variables:**

- Original: EDUC, EXPER, METRO, FEMALE (4 variables)  
- Squared terms: Only EDUC² and EXPER² are valid (METRO² = METRO; FEMALE² = FEMALE)  
- Cross-product terms (6):  
  - EDUC×EXPER  
  - EDUC×METRO  
  - EDUC×FEMALE  
  - EXPER×METRO  
  - EXPER×FEMALE  
  - METRO×FEMALE

**Total degrees of freedom:**

4 (original) + 2 (squares) + 6 (cross terms) = **12**

**Test statistic:**  
NR² = 78.82  
χ²<sub>0.95,12</sub> = 21.026

**Conclusion:**  
78.82 > 21.026 → **reject H₀**  
→ Confirms heteroskedasticity exists in the model

Consistent with parts (b) and (c).

## e
Comparison of OLS vs Robust Standard Errors

| Variable | OLS SE | Robust SE | Change    |
|----------|--------|-----------|-----------|
| EDUC     | 0.14   | 0.16      | Wider     |
| EXPER    | 0.031  | 0.029     | Narrower  |
| METRO    | 1.05   | 0.84      | Narrower  |
| FEMALE   | 0.81   | 0.80      | Narrower  |

**Conclusion:**

- EDUC: robust SE > OLS SE → confidence interval widens  
- Others: robust SE < OLS SE → confidence interval narrows  
- Varying degrees of heteroskedasticity affect SE differently

## f
Is the t-value ≈ 1 for MARRIED inconsistent with (b)?

**Context:**

- df = 1000 − 6 = 994, α = 0.05 → critical value = ±1.96  
- |t| = 1.0 < 1.96 → not significant at 5%

**Interpretation:**

- Coefficient of MARRIED is **not** significant for wage (when controlling for other variables)

**Clarification:**

- (b): tests for **error variance difference**  
- (f): tests whether **MARRIED significantly explains wage**

**Conclusion:**

No contradiction.  
MARRIED may not significantly affect wages directly,  
but its error variance can still be higher → reasonable.

# 8.16
![image](https://github.com/user-attachments/assets/1002cb28-7f34-4186-8539-c6556f7a9742)
## a
OLS Regression and Confidence Interval Estimation

**Regression Result (Focus on KIDS variable):**

- Estimated Coefficient (β̂₄): **-81.826**
- Standard Error (SE): **27.130**
- t test = **1.97214**
- 95% Confidence Interval:
  \[
  [-135.3298, -28.32302]
  \]

**Conclusion:**

Controlling for INCOME and AGE, each additional child is associated with a reduction of approximately **81.826 miles** in annual travel distance. This estimate is statistically significant at the 95% confidence level, as the confidence interval does not include 0.

## b
Graphical Check for Heteroskedasticity: Residual Plots

### 1. Residuals vs INCOME
### 2. Residuals vs AGE

**Observations:**

- **Residuals vs INCOME:**  
  Residuals show greater variation and a funnel-like pattern at higher income levels, suggesting **possible heteroskedasticity**.

- **Residuals vs AGE:**  
  Residuals are relatively evenly spread but include some outliers.

**Conclusion:**

The plot indicates a widening variation of residuals with income, suggesting **potential heteroskedasticity** in the regression model. This pattern is not obvious with AGE, implying the issue is related to INCOME rather than AGE.

## c
Goldfeld–Quandt Test for Heteroskedasticity

**Test Steps:**

1. Sort the sample by INCOME
2. Split into the lowest 90 and highest 90 observations
3. Perform OLS for each group and compute SSE
4. Compute F-statistic:

**Results Summary:**

| Item | Value |
|------|-------|
| SSE (Low income group) | 101744.65 |
| SSE (High income group) | 315821.55 |
| F statistic | **3.104** |
| Critical Value (α = 0.05, df₁ = 86, df₂ = 86) | **1.4286** |

**Hypotheses:**

- H₀: σ²₁ = σ²₂ (Equal variance)
- H₁: σ²₂ > σ²₁ (Heteroskedasticity, higher variance in high-income group)

**Conclusion:**

Since \( F = 3.104 > 1.4286 \), **reject H₀**.  
At the 5% significance level, we have sufficient evidence to suggest **heteroskedasticity** exists in the model.

## d
Re-estimation Using White Robust Standard Errors

**Model Result (KIDS):**

- Estimate (β̂₄): **-81.826**
- Robust Standard Error (HC1): **29.154**
- 95% Confidence Interval:
  \[
  [-138.969, -24.684]
  \]

**Comparison:**

| Model        | Estimate | SE     | 95% Confidence Interval      |
|--------------|----------|--------|------------------------------|
| OLS (a)      | -81.826  | 27.130 | [-135.329, -28.323]          |
| Robust OLS   | -81.826  | 29.154 | [-138.969, -24.684]          |

**Conclusion:**

- **Same estimate**: Model is unchanged
- **Larger SE**: Robust SE accounts for heteroskedasticity
- **Wider CI**: Reflects more conservative and accurate inference

## e
GLS and Robust GLS Estimation: Assuming Var(eᵢ) = σ²·INCOMEᵢ²

Assume:

$$
\text{Var}(e_i) = \sigma^2 \cdot \text{INCOME}_i^2
$$

Apply **Generalized Least Squares (GLS)** and **Robust SE (HC1)** for improved inference.

**KIDS Variable Results:**

| Model        | Estimate | SE      | 95% Confidence Interval      |
|--------------|----------|---------|------------------------------|
| OLS (a)      | -81.826  | 27.130  | [-135.329, -28.323]          |
| Robust OLS   | -81.826  | 29.154  | [-138.969, -24.684]          |
| GLS          | -76.806  | 21.848  | [-119.894, -33.718]          |
| Robust GLS   | -76.806  | 22.618  | [-121.139, -32.474]          |

**Interpretation:**

- GLS estimate is slightly smaller than OLS due to weighting (1/INCOME²)
- GLS SE is smaller → more efficient inference
- Confidence intervals are narrower in GLS and Robust GLS, indicating more precise estimates

# 8.18
![image](https://github.com/user-attachments/assets/a41711b2-aa70-4ba9-b8f5-e26a91923f52)
## a
Goldfeld–Quandt Test: Testing Error Variance Equality between Males and Females

**Hypotheses:**

- H₀: σ²_M = σ²_F (Homoskedasticity)
- H₁: σ²_M ≠ σ²_F (Heteroskedasticity)

**Calculation Results:**

| Item                | Value             |
|---------------------|------------------|
| SSE (Male)          | 1208.99          |
| SSE (Female)        | 925.18           |
| F Statistic         | **1.0538**       |
| Degrees of Freedom  | (5419, 4370)     |
| Critical Values     | [0.9453, 1.0581] |

**Conclusion:**  
F = 1.0538 lies **within** the non-rejection region (0.9453 < 1.0538 < 1.0581).  
→ **Fail to reject H₀**.  
No evidence of different error variances between males and females at the 5% significance level.

## b
NR² Test: Testing for Heteroskedasticity Related to Specific Variables

Variables: METRO, FEMALE, BLACK

**Hypotheses:**

- H₀: Errors are not related to these variables (homoskedasticity)
- H₁: Errors are related to at least one of them (heteroskedasticity)

**Test Results:**

| Item             | Value      |
|------------------|------------|
| Sample Size (n)  | 9799       |
| R² from residual regression | 0.0024 |
| NR² Statistic    | **23.5568**|
| χ² Critical Value (α = 0.01) | **11.3449** |

**Conclusion:**  
Since NR² = 23.5568 > 11.3449 → **Reject H₀**.  
Errors are significantly related to METRO, FEMALE, or BLACK → **Evidence of heteroskedasticity**.

## c
White Test for General Heteroskedasticity

**Hypotheses:**

- H₀: Constant error variance (homoskedasticity)
- H₁: Non-constant error variance (heteroskedasticity)

**Test Results:**

| Metric        | Value         |
|---------------|---------------|
| R²            | 0.0198        |
| NR² Statistic | 194.4447      |
| F Statistic   | 4.4880        |
| F p-value     | **0.0001076** |

**Conclusion:**  
p < 0.01 → **Reject H₀**.  
Model exhibits **clear heteroskedasticity**. Recommend using robust SE or FGLS.

## d
Comparison: OLS vs White Robust Standard Errors

| Variable    | OLS SE  | Robust SE | % Change |
|-------------|---------|-----------|----------|
| Intercept   | 0.0321  | 0.0328    | +2.12%   |
| educ        | 0.0018  | 0.0019    | +8.39%   |
| exper       | 0.0013  | 0.0013    | +1.12%   |
| exper²      | 0.0000  | 0.0000    | +4.71%   |
| female      | 0.0095  | 0.0095    | −0.43%   |
| black       | 0.0169  | 0.0161    | −5.01%   |
| metro       | 0.0123  | 0.0116    | −5.89%   |
| south       | 0.0136  | 0.0139    | +2.51%   |
| midwest     | 0.0141  | 0.0137    | −2.69%   |
| west        | 0.0144  | 0.0146    | +1.07%   |

**Conclusion:**  
Robust SE corrects for heteroskedasticity.  
Increases or decreases are modest but improve inference reliability.

## e
**Steps:**

1. Use OLS residuals ê²
2. Regress log(ê²) on METRO and EXPER
3. Use predicted ĥᵢ for WLS estimation

**FGLS Results:**

| Variable    | Estimate | SE     | 95% CI                      |
|-------------|----------|--------|-----------------------------|
| Intercept   | 1.1922   | 0.0316 | [1.1303, 1.2541]            |
| educ        | 0.1017   | 0.0018 | [0.0982, 0.1051]            |
| exper       | 0.0301   | 0.0013 | [0.0275, 0.0326]            |
| exper²      | -0.00046 | 0.00003| [-0.00051, -0.00040]        |
| female      | -0.1662  | 0.0095 | [-0.1848, -0.1476]          |
| black       | -0.1109  | 0.0170 | [-0.1442, -0.0775]          |
| metro       | 0.1178   | 0.0115 | [0.0953, 0.1402]            |
| south       | -0.0448  | 0.0135 | [-0.0713, -0.0183]          |
| midwest     | -0.0632  | 0.0140 | [-0.0906, -0.0358]          |
| west        | -0.0055  | 0.0144 | [-0.0337, 0.0227]           |

## f
| Variable    | Coef     | FGLS SE | Robust SE | % Change |
|-------------|----------|---------|-----------|----------|
| Intercept   | 1.1922   | 0.0316  | 0.0324    | +2.43%   |
| educ        | 0.1017   | 0.0018  | 0.0019    | +7.26%   |
| exper       | 0.0301   | 0.0013  | 0.0013    | +0.55%   |
| exper²      | -0.0005  | 0.0000  | 0.0000    | +2.31%   |
| female      | -0.1662  | 0.0095  | 0.0094    | −0.45%   |
| black       | -0.1109  | 0.0170  | 0.0159    | −6.61%   |
| metro       | 0.1178   | 0.0115  | 0.0116    | +0.90%   |
| south       | -0.0448  | 0.0135  | 0.0138    | +2.31%   |
| midwest     | -0.0632  | 0.0140  | 0.0137    | −1.94%   |
| west        | -0.0055  | 0.0144  | 0.0145    | +0.92%   |

**Conclusion:**  
FGLS + Robust SE offers balanced efficiency and robustness.  
Recommended for models with confirmed heteroskedasticity.

## g
| Method               | Pros                                              | Cons                                                  |
|----------------------|---------------------------------------------------|--------------------------------------------------------|
| OLS                  | Simple and interpretable, efficient under homoskedasticity | Biased inference under heteroskedasticity              |
| OLS + Robust SE      | Corrects SE under heteroskedasticity             | Less efficient estimates                               |
| FGLS                 | Handles known heteroskedasticity structure       | Sensitive to misspecification                          |
| FGLS + Robust SE **  | Combines efficiency and robustness                | More complex, needs variance modeling                  |

**Recommendation:**  
**FGLS + Robust SE** is preferred due to its robustness and efficiency, especially in presence of heteroskedasticity as indicated by tests in parts (b) and (c).
