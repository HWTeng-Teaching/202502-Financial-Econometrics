## Special Question: Q.02

## Let K=2, show that cov(b1, b2) in p. 30 of slides in Ch 5 reduces to the formula of in (2.14) – (2.16).

#### CH05 P.30

![image](https://github.com/user-attachments/assets/d85b12e6-7e20-4cf6-8d94-86888eb810d3)

#### CH02 2.14-2.16

![image](https://github.com/user-attachments/assets/b9d74ece-7cb4-431e-9ac4-cc355265afc4)


## Ans:
 
we use the formula:

$$
\text{Cov}(b) = (X'X)^{-1}\sigma^2
$$

Here, $\sigma^2$ is the variance of the error terms $e$ in the model $Y = X\beta + e$. The matrix $(X'X)^{-1}$ was previously calculated as:

$$
(X'X)^{-1} = \frac{1}{\Delta} \begin{pmatrix}
\sum x_{i,2}^2 & -\sum x_{i,2} \\
-\sum x_{i,2} & N
\end{pmatrix}
$$

where $\Delta = N \sum x_{i,2}^2 - (\sum x_{i,2})^2$.

### Formula for $\text{Cov}(b)$

Multiplying $(X'X)^{-1}$ by $\sigma^2$, we obtain:

$$
\text{Cov}(b) = \sigma^2 \frac{1}{\Delta} \begin{pmatrix}
\sum x_{i,2}^2 & -\sum x_{i,2} \\
-\sum x_{i,2} & N
\end{pmatrix}
= \frac{\sigma^2}{\Delta} \begin{pmatrix}
\sum x_{i,2}^2 & -\sum x_{i,2} \\
-\sum x_{i,2} & N
\end{pmatrix}
$$

### Elements of the Covariance Matrix

This results in the covariance matrix $\text{Cov}(b)$ having the following elements:

- **Variance of $b_1$ (intercept)**:

$$
\text{Var}(b_1) = \frac{\sigma^2 \sum x_{i,2}^2}{\Delta}
$$

- **Variance of $b_2$ (slope)**:

$$
\text{Var}(b_2) = \frac{\sigma^2 N}{\Delta}
$$

- **Covariance of $b_1$ and $b_2$**:

$$
\text{Cov}(b_1, b_2) = \frac{-\sigma^2 \sum x_{i,2}}{\Delta}
$$


![image](https://github.com/user-attachments/assets/4f29c301-4e5e-4a3e-ba8c-cfcbf9cedf3d)


Thus, the covariance matrix of $b$ captures how the variances of the estimates of the intercept and slope are scaled by $\sigma^2$ and adjusted for the design matrix $X$ through $(X'X)^{-1}$. The off-diagonal elements represent the covariance between the intercept and the slope, indicating how changes in one parameter estimate are statistically associated with changes in the other.
