***學號：513717026     財金碩專一 : 黃德榮***

![C08Q14](https://github.com/user-attachments/assets/d212a62c-b6a7-4d43-aa3c-054df7df3341) 

a.

|             | Estimate | Std. Error | t value | Pr(>\|t\|)    |
|-------------|----------|------------|---------|---------------|
| (Intercept) | -391.548 | 169.775    | -2.306  | 0.0221*       |
| INCOME      | 14.201   | 1.800      | 7.889   | 2.10e-13***   |
| AGE         | 15.741   | 3.757      | 4.189   | 4.23e-05***   |
| KIDS        | -81.826  | 27.130     | -3.016  | 0.0029**      |

confidence level.

|             |2.5 %      |97.5 %      |
|-------------|-----------|------------|
| (Intercept) |-726.36871 |-56.72731   |
| INCOME      |10.65097   |17.75169    | 
| AGE         |8.33086    |23.15099    |
| KIDS        |-135.32981 |-28.32302   |

One more child in household with holding two variables constant decreases approximately 82 miles traveled under a 95% confidence interval ranging from –135 to –28 miles.

b.

Residuals vs. Income:

![image](https://github.com/user-attachments/assets/b601e023-29cf-40f9-bddb-9d9578faddbb)

It is fan-out pattern. When income is increased, residual is getting bigger or smaller.

Residuals vs. Age:

![image](https://github.com/user-attachments/assets/bb18a324-0379-417c-9e6a-2d60286965e6)

There is no clear relationship between the magnitude of the residuals and age.

The heteroskedasticity is shown in the relationship between residuals and income.

c. 

Hypothesis:

- $H_0$: $\sigma^2_{\text{low income}} = \sigma^2_{\text{high income}}$ (homoskedasticity)
- $H_1$: $\sigma^2_{\text{high income}} \neq \sigma^2_{\text{low income}}$ (heteroskedasticity)

alpha = 0.5, 

fstat = $\sigma^2_{\text{low income}} / \sigma^2_{\text{high income}}$ = 3.104061

fuc (Righ critical F) = 1.530373

flc (Left critical F) = 0.6534355

fstat > fuc, reject $H_0$

There is strong evidence indicating that the variance is income-dependent, suggesting the presence of heteroskedasticity.

