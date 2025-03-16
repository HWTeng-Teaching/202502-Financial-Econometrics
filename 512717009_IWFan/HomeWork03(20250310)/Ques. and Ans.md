## Q.01

![image](https://github.com/user-attachments/assets/3fccaf4f-5899-41c1-9ac4-415a63796eed)

## Ans:
**(a)**

Null and Alternative Hypotheses

Null Hypothesis (H₀): β₂ = 0 (GDPB has no effect on MEDALS)

Alternative Hypothesis (H₁): β₂ > 0 (GDPB has a positive effect on MEDALS)

--------------------------------------------------------------

**(b)**

Test Statistic Calculation

t = (b₂ - 0) / se(b₂)

t = (0.01309 - 0) / 0.00215 = 6.09

If H₀ is true, t follows a t-distribution with df = 62.

--------------------------------------------------------------

**(c)**

Distribution Shift Under H₁

When H₀ is true, E(b₂) = 0. When H₁ is true, E(b₂) > 0, so the distribution shifts to the right.

--------------------------------------------------------------

**(d)**
Rejection Region at 1% Significance Level

t_{0.01,62} ≈ 2.39

If t > 2.39, we reject H₀.

--------------------------------------------------------------
**(e)**

Conducting the t-Test and Economic Interpretation

Since t = 6.09 > 2.39, we reject H₀, supporting H₁. GDP positively influences Olympic medal counts.

--------------------------------------------------------------

**Conclusion**

Based on the regression analysis and hypothesis test, we conclude that GDP has a significant positive impact on the number of Olympic medals won by a country. The computed t-statistic (6.09) is much greater than the critical value (2.39) at the 1% significance level, leading us to reject the null hypothesis (H₀: β₂ = 0) in favor of the alternative hypothesis (H₁: β₂ > 0).

Economic Interpretation
Countries with higher GDP tend to win more Olympic medals.

This could be attributed to better sports infrastructure, higher investments in athlete training, and improved access to resources.

The 1% significance level means that we are 99% confident in our conclusion, with only a 1% chance of making a Type I error (incorrectly rejecting H₀).

Thus, economic strength (measured by GDP) appears to be a key factor influencing Olympic success.


--------------------------------------------------------------

## Q.07

![image](https://github.com/user-attachments/assets/7e1d36db-a000-41ec-ad37-6a0b7e88be62)

## Ans:
**(a)**

Estimating the Intercept

Given model: INCOME = a + 1.029BACHELOR

t = a / se(a)

Given t = 4.31, se(a) = 2.672, so a = 4.31 × 2.672 ≈ 11.52

--------------------------------------------------------------

**(b)**

![image](https://github.com/user-attachments/assets/fd9c78f4-4753-47e0-ac26-b77278398b56)

Relationship and Trend

BACHELOR and INCOME are positively correlated. Each 1% increase in BACHELOR raises INCOME by 1.029 thousand dollars.

--------------------------------------------------------------

**(c)**

Standard Error of the Slope

se(b₂) = 1.029 / 10.75 ≈ 0.0958

--------------------------------------------------------------

**(d)**

t-statistic for Intercept (H₀: a = 10)

t = (11.52 - 10) / 2.672 ≈ 0.569

--------------------------------------------------------------

**(e)**

p-value and Rejection Region

p-value = 0.572, greater than 0.05, so we fail to reject H₀

--------------------------------------------------------------

**(f)**

99% Confidence Interval for Slope

CI = 1.029 ± (2.68 × 0.0958) ≈ (0.773, 1.285)

--------------------------------------------------------------

**(g)**

Hypothesis Test for Slope (H₀: β₂ = 1)

t = (1.029 - 1) / 0.0958 ≈ 0.303, so we fail to reject H₀

--------------------------------------------------------------

**Conclusion**

Based on the regression analysis, we conclude that the percentage of the population with a bachelor's degree (BACHELOR) has a significant positive impact on income per capita (INCOME). The computed t-statistics and confidence intervals confirm that the relationship between education and income is statistically significant.

--------------------------------------------------------------


## Q.17

![image](https://github.com/user-attachments/assets/e6afebd7-9f03-42a6-8e82-88c30bb45c63)

## Ans:
**(a)**
Testing if the Urban Slope Equals 1.80

H₀: β₂ = 1.80, H₁: β₂ > 1.80

t = (2.46 - 1.80) / 0.16 = 4.125

t_{0.05, 985} ≈ 1.645, 4.125 > 1.645, reject H₀

--------------------------------------------------------------

**(b)**
Computing the 95% Confidence Interval for WAGE at EDUC = 16 in Rural Areas

WAGE = -4.88 + 1.80(16) = 23.92

CI = 23.92 ± (1.97 × 0.833) = (22.279, 25.561)

--------------------------------------------------------------

**(c)**
Computing the 95% Confidence Interval for WAGE at EDUC = 16 in Urban Areas

WAGE = -10.76 + 2.46(16) = 28.6

CI = 28.6 ± (1.96 × 0.833) = (26.967, 30.233)

--------------------------------------------------------------

**(d)**
Testing if the Rural Intercept is Less Than 4

H₀: β₁ ≥ 4, H₁: β₁ < 4

t = (-4.88 - 4) / 3.29 = -2.7

t_{0.01, 213} ≈ -2.33, -2.7 < -2.33, reject H₀

--------------------------------------------------------------

**Conclusion**

Based on the regression analysis, we conclude that education has a significant positive effect on wages, with different impacts in urban and rural areas. The computed t-statistics, confidence intervals, and hypothesis tests confirm that schooling is an important determinant of income.

--------------------------------------------------------------

## Q.19

![image](https://github.com/user-attachments/assets/a33ba53b-1bdc-4f4e-990a-d0bccf8e3052)

## Ans:
**(a)**
(a) Plotting MOTEL_PCT and COMP_PCT and Analyzing Trends

We first plot MOTEL_PCT (motel occupancy rate) and COMP_PCT (competitor occupancy rate) on the same time series graph to observe their trends.

![image](https://github.com/user-attachments/assets/f6463636-dcc6-43fa-ba59-99ca93e846cc)

Observations:

Synchronized movement: The graph shows that MOTEL_PCT and COMP_PCT exhibit a degree of correlation over time. When the competitor’s occupancy rate rises, the motel’s occupancy rate tends to increase as well.

Higher competitor occupancy: COMP_PCT is consistently higher than MOTEL_PCT, suggesting that the competitor may have brand advantages, a better location, or more competitive pricing.

Time-based variations:
Around period 10: MOTEL_PCT exhibits noticeable fluctuations, possibly due to seasonal effects, holidays, promotions, or changes in market demand.

Periods 17-23 (July 2004 - January 2005): A sharp decline in occupancy rates is observed, which may be related to economic downturns or motel-specific issues such as repairs.
Economic Interpretation:

The observed synchronization suggests that the competitor’s occupancy rate influences motel demand, possibly indicating:

A weak substitution effect, meaning customers do not necessarily switch away from the motel when the competitor's occupancy rate increases.
Changes in overall market demand, implying that when competitors perform well, the entire market may be experiencing higher demand rather than just competitive shifts.

--------------------------------------------------------------

**(b)**
Calculating the 90% Confidence Interval for MOTEL_PCT when COMP_PCT = 70Using the regression model:

![image](https://github.com/user-attachments/assets/f38de687-3b13-494f-b7bb-a5dc6979ddcb)

we predict the expected motel occupancy rate when the competitor's occupancy rate is 70%.

Results:

Point Estimate (Predicted Value):

![image](https://github.com/user-attachments/assets/a55d7c3f-03fb-4809-8f9b-03b6d97ed1e1)

90% Confidence Interval Calculation:

![image](https://github.com/user-attachments/assets/cc5fea71-1ea0-4b6d-bb27-e20c77afd489)

Numerical Example： CI=(77.38%,86.47%)

Economic Interpretation:

We are 90% confident that when the competitor’s occupancy rate is 70%, the motel’s occupancy rate will be between 77.38% and 86.47%.
This implies:

Market demand stability: Despite a higher competitor occupancy rate, the motel maintains a significant share of demand.
Competitive influence: If COMP_PCT declines, MOTEL_PCT may also drop, highlighting the interdependence between market competitors.

--------------------------------------------------------------

**(c)**
Hypothesis Test: H₀: β₂ ≤ 0, H₁: β₂ > 0

We conduct a one-tailed test to examine whether the competitor’s occupancy rate significantly influences the motel’s occupancy rate.

Testing Steps:

Hypotheses:

Null Hypothesis (H₀): β₂ ≤ 0 (The competitor’s occupancy rate has no effect on the motel’s occupancy rate)
Alternative Hypothesis (H₁): β₂ > 0 (The competitor’s occupancy rate positively influences the motel’s occupancy rate)

Calculate the t-statistic:

![image](https://github.com/user-attachments/assets/5e8998ea-c17a-4efb-ad0b-2c019d3e9dc0)

Compare with critical value (α = 0.01). If t is greater than the critical value, we reject H₀.

Results:

t = 4.265

Critical Value = 2.499867

Conclusion: Since 4.265 > 2.499867, we reject H₀, indicating that β₂ is significantly greater than 0.

Economic Interpretation:

The competitor’s occupancy rate significantly affects the motel’s occupancy rate, showing strong market interdependence.
Weak substitution effect: Customers do not completely switch away from the motel when the competitor's occupancy rate rises, suggesting overall market demand changes rather than pure competition effects.

--------------------------------------------------------------

**(d)**
Hypothesis Test: H₀: β₂ = 1, H₁: β₂ ≠ 1

We conduct a two-tailed test to determine whether β₂ is equal to 1, meaning whether the motel's occupancy rate changes at the same rate as the competitor’s.

Results:

t = -0.6677491

Critical Value = ±2.807336

Since t falls within (-2.807336, 2.807336), we fail to reject H₀.

Economic Interpretation:

This suggests that a 1% increase in the competitor’s occupancy rate results in a nearly 1% increase in the motel’s occupancy rate.
This indicates:

Market-wide demand fluctuations affect all competitors similarly, rather than substitution effects.
The motel's market share remains stable relative to the competitor’s performance.

--------------------------------------------------------------

**(e)**
Residual Analysis

We compute the residuals:

![image](https://github.com/user-attachments/assets/a30e3e50-ebe2-4ce9-96f8-3d64db774a49)

and analyze their time-series pattern.

![image](https://github.com/user-attachments/assets/653d2c57-8141-415c-b8c7-d0ac967001aa)

Findings:

Periods 17-23 (July 2004 - January 2005):

Residuals are predominantly negative, indicating that the regression model overestimated the motel's occupancy rate.
This could be linked to market disruptions such as economic downturns, external shocks, or motel repairs.

Economic Interpretation:

Model overestimation suggests that market demand was unusually weak during these periods.
Price fluctuations were not accounted for: Including price variables might improve the model’s predictive power.

--------------------------------------------------------------




