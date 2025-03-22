# 3.1
![image](https://github.com/user-attachments/assets/4f7a52d3-774e-44f1-8d5f-28c9a5ee8f18)
## (a) 設定虛無假設(_H<sub>0</sub>_)和對立假設(_H<sub>1</sub>_)
- 虛無假設(_H<sub>0</sub>_)：GDP和奧運獎牌數無關，即 H<sub>0</sub>：β₂ = 0  
- 對立假設(_H<sub>1</sub>_)：GDP和奧運獎牌數有正向關係，即H<sub>1</sub>：β₂ > 0  
- 說明：右尾檢定（right-tailed test），因為假設 GDP 增加會導致獎牌數增加
## (b) 計算檢定統計量（t-statistic）並確定其分佈
- Formula：t = (b₂ − 0) / se(b₂)  
→ t = 0.01309 / 0.00215 ≈ 6.086  
- 分佈：如果虛無假設成立(即β₂ = 0)，自由度(df) = n - 2 = 64 - 2 = 62    
## (c) 若對立假設為真，t 統計量的分佈如何變化
- 若H<sub>0</sub>為真(即β₂ = 0)，則 t 統計量應該遵循標準的 t 分佈，平均值為 0  
- 若H<sub>1</sub>為真(即β₂ > 0)，則 t 統計量的值將變大（正向移動），因為b₂將會大於 0，導致 t 值增加，使其偏離 t 分佈的中心(向右移動)  
## (d) 1% 顯著性水準下，拒絕H<sub>0</sub>的t值臨界點(critical value)
- 顯著水準 𝛼 = 0.01(單尾檢定)，自由度(df) = 62，t<sub>62,0.99</sub> = 2.39  
- 拒絕域(reject region) = t > 2.39
- 若t值落在此區間，則拒絕H<sub>0</sub>，反之則不拒絕H<sub>0</sub>
## (e) 進行 t 檢定/說明經濟含義與顯著水準(1%)
- t = 6.086 > 臨界點(critical value) = 2.39；拒絕虛無假設(H<sub>0</sub>)
- 經濟含義：GDP 較高的國家往往能贏得較多的獎牌，可能是因為這些國家擁有更多的運動資源、良好的基礎設施、更完善的運動訓練體系等
- 顯著水準(1%)：拒絕虛無解假設(H<sub>0</sub>)的錯誤率只有 1%  
## *R code*
<img width="356" alt="image" src="https://github.com/user-attachments/assets/0fe72546-ef36-48ce-aa2f-d4829a67959b" />

---
# 3.7
![image](https://github.com/user-attachments/assets/ec549157-f45c-4138-a631-4a597a546c92)
## (a)
The regression equation is:

  INCOME = (a) + 1.029 × BACHELOR  
The provided t-value for the intercept is 4.31 with a standard error of 2.672.  
Using the formula:  
  t = (estimated intercept − 0) / standard error  
Thus, estimated intercept = t × standard error = 4.31 × 2.672 ≈ 11.51632  

* The estimated intercept is approximately 11.53.
## (b)
The estimated regression is:

  INCOME = 11.53 + 1.029 × BACHELOR  
<img src="https://github.com/user-attachments/assets/6ed35ff5-b685-4bc7-b0c2-2d2ac9d9410a" alt="圖片描述" width="400" height="250" />

**Interpretation:**  
Since the slope (1.029) is positive, as BACHELOR increases, INCOME increases.  
This indicates a positive relationship. Being a linear model, the rate of increase is constant.

**Sketch Concept:**  
Plot a straight line with the horizontal axis representing BACHELOR and the vertical axis representing INCOME, starting at approximately 11.53 and rising by about 1.029 units for every one-unit increase in BACHELOR.
## (c)
Given slope = 1.029 and its t-value = 10.75, use the formula:  

  t = (slope − 0) / standard error  
Thus, standard error = slope / t = 1.029 / 10.75 ≈ 0.0957  

* The standard error of the slope is approximately 0.0958.
## (d)
Under the null hypothesis, intercept = 10. The estimated intercept is approximately 11.53 with a standard error of 2.672.  
Calculation:  
  t = (11.5163 − 10) / 2.672 ≈ 0.567 

*The t-statistic is approximately 0.567.
## (e)
<img src="https://github.com/user-attachments/assets/5e093594-4029-46b0-8627-ab84365703ac" alt="圖片描述" width="400" height="250" />

* the rejection regions are for t < −2.01 or t > 2.01, the observed t-value falls well within the acceptance region.
## (f)
The 99.5 Percentile of t(49) distribution: 2.68

The 99% interval estimated for the slope :

$$
\ b_2 ± t_c se(b_2) = 1.029 ± 2.68(0.0957) = [0.7725,1.2855].\
$$
## (g)
**Hypotheses:**
* Null Hypothesis H0: β₂ = 1
* Alternative Hypothesis H1: β₂ ≠ 1
* 
**Test Statistic:**

$$
t = \frac{b_2 - 1}{\mathrm{SE}(b_2)}    ,t(n-2)
$$

if the null hypothesis is true. The 97.5 percentile of the t(49) distribution  is 2.009, so we reject the null hypothesis of the t-value is greater than or equal to 2.0096 or if t is less than or equal to −2.009. The calculated t-value is 

* t = (1.029 - 1) / (0.0957) = 0.303

Therefore, we fail to reject the null hypothesis H0: β₂ = 1. We cannot reject the null hypothesis that each additional 1% percentage of the population with a bachelor’s degree increases per capita income by $1000.

---
# 3.17
![image](https://github.com/user-attachments/assets/95bb8fb4-12ff-40a9-984b-96a75a51008f)
## (a)
### 設定虛無假設(_H<sub>0</sub>_)和對立假設(_H<sub>1</sub>_)
- 虛無假設(_H<sub>0</sub>_)：β₂ = 1.8
- 對立假設(_H<sub>1</sub>_)：β₂ > 1.8
### 計算檢定統計量(t-value)
- t = (b₂ − 0) / se(b₂) = (2.46 − 1.80) / 0.16 ≈ 4.125
### 查找 t 臨界值(critical value)
- 顯著水準 𝛼 = 0.05(單尾檢定)，自由度(df) = 986 - 2 = 984
- t<sub>984,0.95</sub> = 1.645
### 決策
- 若𝑡 > 1.645，則拒絕
- t = 4.125 >> crtical value = 1.645，拒絕虛無假設(_H<sub>0</sub>_)
### 結論
- 在95%信心水準下，urban regression 斜率將顯著大於1.8
## (b)
- Rural點估計(EDUC=16)：WAGE = −4.88 + 1.80(16) = −4.88 + 28.8 = 23.92
- 標準誤 = 0.833 (題目給訂)
- 自由度(df) = 214 - 2 = 212
- t<sub>0.975,212</sub> ≈ 1.97
- 信賴區間(CI) = 23.92 ± 1.97 × 0.833 → CI=(22.28,25.56)
## (c)
- Urban點估計(EDUC=16)：WAGE = –10.76 + 2.46(16) = 28.60
- 標準誤 = √[ (2.27)² + (16)² × (0.16)² + 2×16×(–0.345) ] ≈ 0.816
- 自由度(df) = 986 − 2 = 984
- t<sub>0.975,984</sub> ≈ 1.97
- 信賴區間(CI) = 28.60 ± 1.97 × 0.816 → CI=(26.99,30.21)
- 說明(Rural VS Urban)：Urban區間較窄，因為樣本數較多(結果較精確)
## (d)
### 設定虛無假設(_H<sub>0</sub>_)和對立假設(_H<sub>1</sub>_)
- 虛無假設(_H<sub>0</sub>_)：β<sub>1</sub> ≥ 4  
- 對立假設(_H<sub>1</sub>_)：β<sub>1</sub> < 4  
### 計算檢定統計量(t-value)
- t = (b1 − 0) / se(b1) = (−4.88 − 4) / 3.29 ≈ −2.70
### 查找 t 臨界值(critical value)
- 顯著水準 𝛼 = 0.05，自由度(df) = 212
- t<sub>212,0.95</sub> = −2.33
### 決策
- 若𝑡 < −2.33，則拒絕
- t = −2.70 << crtical value = −2.33，拒絕虛無假設(_H<sub>0</sub>_)
### 結論
- 農村的截距顯著小於 4，表示當教育程度為 0 時，預測工資低於 4 美元
---
# 3.19
![image](https://github.com/user-attachments/assets/68121018-af9b-484e-a7d7-558db45047b2)
## (a)
<img src="https://github.com/user-attachments/assets/64955f9d-a75c-49b8-902d-52d48edca613" alt="圖片描述" width="400" height="250" />

### Plotting and Trend Analysis
- **Plot the Data:**  
  Plot the two variables, **MOTEL_PCT** and **COMP_PCT**, against **TIME** on the same graph.
- **Visual Comparison:**  
  By visually comparing the curves, you can determine whether the occupancy rates move together over time.  
  - If both series follow a similar pattern (for example, both rising or falling simultaneously), it suggests that external factors (such as seasonality) affect them similarly.
  - Observe which series tends to have higher occupancy; for instance, the competitor’s occupancy might consistently be higher than the motel’s.

  Estimate the regression model:
MOTEL_PCT = β₁ + β₂ COMP_PCT + e

- **Parameter Estimates:**  
Obtain the estimates **β̂₁** and **β̂₂** along with the standard error for **β̂₂**.

### 95% Confidence Interval for β₂
- **Interval Formula:**  
The confidence interval is given by:

β̂₂ ± t(0.975, df) × SE(β̂₂)


- **Interpretation:**  
A narrow confidence interval indicates that the association between **MOTEL_PCT** and **COMP_PCT** has been estimated precisely, whereas a wide interval suggests a high level of uncertainty in the estimate.

## (b)
### Prediction
- **Calculate the Predicted Value:**  
Use the estimated regression model to predict **MOTEL_PCT** when **COMP_PCT = 70**:

MOTEL_PCT^ = β̂₁ + β̂₂ × 70

### Constructing the Interval
- **Confidence Interval Formula:**  
The 90% confidence interval for the expected value is:

MOTEL_PCT^ ± t(0.95, df) × SE(MOTEL_PCT^)


where **SE(MOTEL_PCT^)** is the standard error of the prediction.
- **Interpretation:**  
This interval provides the range within which we are 90% confident that the true mean occupancy rate will fall when **COMP_PCT** is 70.

## (c)
### Hypotheses
- **Null Hypothesis (H₀):** β₂ ≤ 0  
- **Alternative Hypothesis (Hₐ):** β₂ > 0

### Test Statistic
- **Calculation:**  
Compute the t-statistic as:

t = (β̂₂ − 0) / SE(β̂₂)


### Rejection Region
- For a one-tailed test at **α = 0.01**, reject H₀ if:

t > t(0.99, df)


### Conclusion
- If the computed t-statistic exceeds the critical value, you reject H₀ and conclude that there is statistically significant evidence at the 1% level that the association between **COMP_PCT** and **MOTEL_PCT** is positive.

## (d)
### Hypotheses
- **Null Hypothesis (H₀):** β₂ = 1  
- **Alternative Hypothesis (Hₐ):** β₂ ≠ 1

### Test Statistic
- **Calculation:**  
Compute the t-statistic as:

t = (β̂₂ − 1) / SE(β̂₂)


### Rejection Region
- For a two-tailed test at **α = 0.01**, reject H₀ if:

|t| > t(0.995, df)


### Interpretation
- If H₀ were true (β₂ = 1), it would imply a one-for-one relationship between competitor occupancy and the motel’s occupancy (i.e., a 1 percentage point increase in **COMP_PCT** would lead to a 1 percentage point increase in **MOTEL_PCT**).
- If the t-statistic falls in the rejection region, conclude that the relationship deviates significantly from one-for-one.

## (e)
<img src="https://github.com/user-attachments/assets/f8b45775-c411-45ae-a1fc-2aa9384a4c72" alt="圖片描述" width="400" height="250" />

### Calculation of Residuals
- **Residual Formula:**  
For each observation, calculate the residual as:

eᵢ = MOTEL_PCTᵢ − (β̂₁ + β̂₂ COMP_PCTᵢ)


### Plotting vs. TIME
- **Purpose:**  
Plot these residuals against **TIME** to check for any systematic patterns.
- **Ideal Pattern:**  
Ideally, the residuals should be randomly scattered around zero, indicating a good model fit.

### Examination of Time Periods 17–23
- **Focus Period:**  
Analyze the residuals from **TIME periods 17–23** (from July 2004 to January 2005).
- **Interpretation:**  
Determine whether the majority of the residuals in this period are positive or negative.  
- For example, if most residuals are negative, it suggests that the actual motel occupancy was lower than predicted by the regression model during these months—possibly due to disruptions such as construction defect corrections.
