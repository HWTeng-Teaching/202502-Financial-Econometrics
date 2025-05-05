# 10.18
![image](https://github.com/user-attachments/assets/6637ba10-965f-477b-9e89-4c7a3f391359)

## (a) Proportion with Some College
- **Mothers:** 12.15 %  
- **Fathers:** 11.68 %

## (b) Correlations
| Pair                       | ρ     |
|----------------------------|-------|
| EDUC – MOTHERCOLL          | 0.359 |
| EDUC – FATHERCOLL          | 0.398 |
| MOTHERCOLL – FATHERCOLL    | 0.355 |

Parents who have attended college are likely to place greater importance on their children's education compared to those who have not attended college.

These moderate correlation coefficients indicate that the instrumental variables exhibit relevance, but are not perfectly collinear.

Using dummy variables such as "whether attended college" helps reduce measurement error and aligns with the assumption that having attended college has a more significant effect than years of education.

## (c) 2SLS with MOTHERCOLL as the Sole Instrument
- β^2_EDUC = 0.0760\)  
- \(SE = 0.042\)  
- 95 % CI = \([−0.006,\;0.158]\)

## (d) First-Stage F-Statistic
- \(F = 63.56\) (p < 0.001) ⇒ strong instrument
The F-statistic is 63.56, which is well above the rule-of-thumb threshold of 10. Therefore, we reject the null hypothesis that the instrument is weak, indicating that the IV is sufficiently relevant.

## (e) 2SLS with MOTHERCOLL & FATHERCOLL
- \(\displaystyle \hat\beta_{\mathrm{EDUC}} = 0.0878\)  
- \(SE = 0.032\)  
- 95 % CI = \([0.024,\;0.151]\)

> The confidence interval is slightly narrower than in (c) IV, indicating higher efficiency of the estimation.

## (f) Joint Strength of Both Instruments
**First-stage test**  
\(H_0: \pi_1 = \pi_2 = 0\)

- \(F = 56.96\) (p < 0.001) ⇒ jointly strong

## (g) Over-Identification (Hansen J)
- \(J = 0.214\)  
- \(df = 1\)  
- \(p = 0.64\)

The regression of all exogenous and instrumental variables yielded an NR2 test statistic of 0.237585. Under the null hypothesis that the instrument is valid, this statistic follows a chi-squared distribution with 1 degree of freedom. Since 0.237585 is well below the 5% critical value of 3.84, we fail to reject the null hypothesis, suggesting that the instrument is valid.

## Summary

- College-educated parents are relatively rare (≈ 12 %).  
- Their college dummies are relevant and strong instruments for **EDUC**.  
- Using both dummies increases precision, and the over-identification test supports instrument validity.

------
# 10.20
![image](https://github.com/user-attachments/assets/c8cff5f1-d0d7-47f7-8f76-e88a9b21d9fc)
## (a) OLS Estimation

$$
r_{\text{MSFT}} - r_f = 0.0033 + 1.2018\,(r_m - r_f)
$$

- **Estimated β:** \(\beta \approx 1.20\) ⇒ Microsoft was slightly riskier than the market.  
- **\(R^2 = 0.352\)**

The 95% confidence interval for the estimate is [0.9607882, 1.442891]. Since most of the interval lies above 1, this suggests that the stock is relatively riskier compared to the market portfolio. However, note that the null hypothesis that Microsoft’s beta equals 1 cannot be rejected at the 5% significance level.

## (b) Instrument: RANK  
**First-stage regression:**  
\[
r_m - r_f = -0.0790 + 0.000907\,\text{RANK}
\]
- **t-stat:** 43.1  
- **\(R^2 = 0.913\)** ⇒ strong IV; IV relevance plausible.

## (c) Exogeneity Test  
Augmented regression including first-stage residual \(\hat v\):  
- **p-value:** 0.0428 > 0.01  
- **Conclusion:** Cannot reject exogeneity of \(r_m - r_f\) at the 1% level.

## (d) 2SLS with RANK  
- \(\displaystyle \hat\alpha_{\text{IV}} = 0.0030\)  
- \(\displaystyle \hat\beta_{\text{IV}} = 1.2783\)  
- \(\hat\beta_{\text{IV}} > \hat\beta_{\text{OLS}}\), consistent with attenuation bias from measurement error.

The 95% confidence interval is now [1.028819,1.527817]. Since all values in the interval are greater than 1, we reject the null hypothesis that Microsoft’s beta equals 1.

## (e) Add Second IV: POS  
- **First-stage F:** 951.3  
- **\(R^2 = 0.915\)** ⇒ instruments jointly very strong.

The F-statistic exceeds 10 and is also greater than the Stock-Yogo critical value of 19.93 based on the test size criterion. If we are willing to accept a 10% Type I error rate for a 5% significance test, we conclude that the instrument is not weak.

## (f) Hausman Test  
- **p-value (residual):** 0.0287 > 0.01  
- **Conclusion:** Fail to reject exogeneity at 1%.

## (g) 2SLS with RANK & POS  
- \(\displaystyle \hat\alpha_{\text{IV},2} = 0.0030\)  
- \(\displaystyle \hat\beta_{\text{IV},2} = 1.2831\)  
- Very close to (d); still larger than OLS as expected.

The estimated coefficient is 1.2866, which is higher than the OLS estimate reported in part (a). The 95% confidence interval is [1.037005, 1.536111], and since all values in the interval are greater than 1, we reject the null hypothesis that Microsoft’s beta equals 1 at the 5% significance level.

## (h) Breusch–Pagan Test for Heteroskedasticity  
\[
\chi^2(1) = 1.57,\quad p = 0.21
\]
- **Conclusion:** No evidence of heteroskedasticity at the 5% level.

Therefore, we reject the null hypothesis of IV validity. This suggests that there may be an issue with the current instrument, and a new, more appropriate IV should be identified.

## Summary
- Microsoft’s beta is approximately **1.20–1.28**, indicating slightly higher systematic risk than the market.  
- The instruments **RANK** and **POS** are strong and jointly relevant.  
- Hausman tests do not reject exogeneity.  
- No heteroskedasticity detected via Breusch–Pagan.  
- Overall, IV estimates confirm and refine the OLS result.

------
# 10.24
![image](https://github.com/user-attachments/assets/daa4f1be-5422-45a0-b5e1-794d6b57a54d)
## Model & Estimation  

$$
\ln(\text{wage}) = \beta_0 + \beta_1\,\text{EDUC} + \beta_2\,\text{EXPER} + \beta_3\,\text{EXPER}^2 + u,
$$

estimated by **two‑stage least squares (2SLS)** using **MOTHEREDUC** and **FATHEREDUC** as instruments for `EDUC`.

## (a) Residual Plot  
Plotting the IV residuals against **EXPER** displays a clear **funnel pattern**—greater spread at low experience levels—suggesting heteroskedasticity.

![image](https://github.com/user-attachments/assets/4e05fabf-230c-4288-b918-b673d10929e5)


## (b) NR² (Breusch–Pagan) Test  

Regress \(\hat u^{2}\) on a constant and **EXPER**:

\[
R^{2}=0.0174,\quad
N R^{2}=428\times0.0174\approx7.44.
\]

The critical value is \(\chi^{2}_{0.95}(1)=3.84\).  
Because \(7.44>3.84\) (*p* ≈ 0.006), **homoskedasticity is rejected**.


## (c) HC0 (White) Robust Standard Errors

| Coefficient | Conventional SE | HC0 SE | 95 % CI (HC0) |
|-------------|-----------------|--------|---------------|
| **EDUC** | 0.0333 | 0.0350 | [−0.004, 0.127] |

Robust SEs exceed classical ones, and the confidence interval includes zero, so **EDUC is not significant at 5 %**.

## (d) Bootstrap (B = 200)

| Coefficient | Bootstrap SE | 95 % CI (Boot) |
|-------------|--------------|----------------|
| **EDUC** | 0.0338 | [−0.003, 0.126] |

The bootstrap SE lies between the classical and HC0 values; its confidence band almost mirrors the HC0 result and likewise covers zero.

## Overall Interpretation  

The presence of heteroskedasticity means conventional standard errors are unreliable. After HC0 or bootstrap correction, the estimated return to education for married women **is not statistically significant at the 5 % level** in this specification.

![image](https://github.com/user-attachments/assets/342d1892-02b9-421d-8214-1320ce76ebfd)
