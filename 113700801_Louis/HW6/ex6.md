We are given:

- Estimated coefficients:  
  **b = (b₁, b₂, b₃) = (2, 3, -1)**

- Estimated covariance matrix:

| 3 -2 1 | | -2 4 0 | | 1 0 3 |


- Number of observations: **n = 63**  
- Number of parameters: **k = 3**  
- Degrees of freedom: **df = 60**


(a) H₀: β₂ = 0

- This is a simple **t-test** on the second coefficient.
- Test statistic:  
  \[
  t = \frac{\hat{\beta}_2 - 0}{\text{SE}(\hat{\beta}_2)}
  \]
- We reject H₀ if the p-value < 0.05.

(b) H₀: β₁ + 2β₂ = 5

- This is a **linear restriction**:  
  \[
  R \beta = q \quad \text{with} \quad R = [1, 2, 0], \quad q = 5
  \]
- The **Wald (F) test** statistic is:  
  \[
  F = \frac{(R\hat{\beta} - q)^2}{R V R'}
  \]
- Rejection region: **F > F₀.₀₅(1, 60)**


(c) H₀: β₁ − β₂ + β₃ = 4

- Another **linear restriction**:  
  \[
  R = [1, -1, 1], \quad q = 4
  \]
- Same Wald F-test approach as above.
