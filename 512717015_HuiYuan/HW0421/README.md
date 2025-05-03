# 10.2
![image](https://github.com/user-attachments/assets/657027af-47f8-43d5-a053-51e5b61ded33)
## a
$\beta_1$ = Zero or positive. Beacuse HOURS can't be negative.<br>
$\beta_2$ = Positive. Because it will be more attractive if the wage is higher.<br>
$\beta_3$ = Positive. Because higher education increases both the willingness and ability to work.<br>
$\beta_4$ = Not sure. As age increases, experience also grows, leading to longer HOURS. However, productivity may decline due to aging.<br>
$\beta_5$ = Negative. Because women may focus on their children rather than working.<br>
$\beta_6$ = Negative. If other family members have sufficient financial resources, women may feel less financial pressure and therefore may not actively participate in the workforce.
## b
wage could be endogenous because it might be influanced by unobservable factors as for exemple the motivation, ability, job exeprience...that could also affect the hours supply.

the error term may capture these unobservable factors (thus E[e∣WAGES]=0 doesn't hold anymore) and so make the estimators biased and inconsistent.
## c
To be valid instruments, EXPER and EXPER² must satisfy:

| Condition | Satisfied? | Explanation |
|-----------|------------|-------------|
| **Relevance** |  Yes | Experience is strongly correlated with wages |
| **Exogeneity** | Reasonable | Experience affects HOURS only indirectly through WAGE |

* Thus, EXPER and EXPER² are **plausible instruments** for WAGE.
## d
- One endogenous regressor: **WAGE**
- Two instruments: **EXPER**, **EXPER²**

Definition:

A model is said to be identified when the number of instruments ≥ the number of endogenous variables.
The model is then considered either just-identified or overidentified.

In this case:

1 endogenous variable

2 instruments

➡️ The model is overidentified

➡️ It is identified, and we can test the validity of the instruments (e.g., Sargan test)
## e
Step 1: First Stage Regression

Regress the endogenous variable **WAGE** on the instruments:

$$
WAGE = \gamma_0 + \gamma_1 \cdot EXPER + \gamma_2 \cdot EXPER^2 + \text{controls} + v
$$

Obtain the fitted values $\widehat{WAGE}$ from this regression.


Step 2: Second Stage Regression

Replace **WAGE** with $\widehat{WAGE}$ in the original model and estimate using OLS:

$$
HOURS = \beta_1 + \beta_2 \cdot \widehat{WAGE} + \beta_3 \cdot EDUC + \beta_4 \cdot AGE + \beta_5 \cdot KIDSL6 + \beta_6 \cdot NWIFEINC + u
$$

The resulting estimate $\hat{\beta}_2$ is the **IV / 2SLS estimate** for the causal effect of WAGE on HOURS.

------
# 10.3
![image](https://github.com/user-attachments/assets/0b065376-c3d2-47f0-bc20-59eb985a134d)
Problem Context

In the regression model:

$$
y = \beta_1 + \beta_2 x + e
$$

assume that $x$ is endogenous and $z$ is a valid instrument.

It is known that:

$$
\beta_2 = \frac{\text{Cov}(z, y)}{\text{Cov}(z, x)}
$$
## a
Consider the simple linear regression:

$$
x = \gamma_1 + \theta_1 z + \nu
$$

The OLS estimator is:

$$
\hat{\theta}_1 = \frac{\text{Cov}(z, x)}{\text{Var}(z)}
$$

Therefore, $\frac{\text{Cov}(z, x)}{\text{Var}(z)} = \theta_1$

This is the **first-stage** regression in two-stage least squares (2SLS).

## b
Consider another simple regression:

$$
y = \pi_0 + \pi_1 z + u
$$

The OLS estimator is:

$$
\hat{\pi}_1 = \frac{\text{Cov}(z, y)}{\text{Var}(z)}
$$

Thus, $\frac{\text{Cov}(z, y)}{\text{Var}(z)} = \pi_1$

This is the **reduced-form** regression of $y$ on $z$.

## c
Start with the structural model:

$$
y = \beta_1 + \beta_2 x + e
$$

Substitute in the first-stage regression: $x = \gamma_1 + \theta_1 z + \nu$:

$$
y = \beta_1 + \beta_2 (\gamma_1 + \theta_1 z + \nu) + e \\
= \beta_1 + \beta_2 \gamma_1 + \beta_2 \theta_1 z + \beta_2 \nu + e
$$

Define:

- $\pi_0 = \beta_1 + \beta_2 \gamma_1$
- $\pi_1 = \beta_2 \theta_1$
- $u = \beta_2 \nu + e$

We get the reduced-form equation:

$$
y = \pi_0 + \pi_1 z + u
$$

## d
From (c), we know:

$$
\pi_1 = \beta_2 \theta_1 \\
\Rightarrow \beta_2 = \frac{\pi_1}{\theta_1}
$$

## e
If $\hat{\pi}_1$ and $\hat{\theta}_1$ are consistent OLS estimators, then:

$$
\hat{\beta}_2 = \frac{\hat{\pi}_1}{\hat{\theta}_1}
$$

Since:

- $\hat{\pi}_1 \xrightarrow{p} \pi_1$
- $\hat{\theta}_1 \xrightarrow{p} \theta_1$

By the **Continuous Mapping Theorem**:

$$
\hat{\beta}_2 = \frac{\hat{\pi}_1}{\hat{\theta}_1} \xrightarrow{p} \frac{\pi_1}{\theta_1} = \beta_2
$$

Hence, $\hat{\beta}_2$ is a consistent estimator of $\beta_2$, known as the **indirect least squares (ILS)** estimator.
