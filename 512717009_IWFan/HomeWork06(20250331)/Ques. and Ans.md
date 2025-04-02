## Q.06

<img src="https://github.com/user-attachments/assets/7ce8ceae-e277-4aed-9209-8ddd3a09b0b0" alt="圖片描述" width="600" height="320" />

## Ans:

Hypothesis Testing with Estimated Coefficient Covariance Matrix

From a sample of 63 observations, the least squares estimates and the estimated covariance matrix are:

estimated：

```math
\begin{bmatrix}
b_1 \\
b_2 \\
b_3
\end{bmatrix}
=
\begin{bmatrix}
2 \\
3 \\
-1
\end{bmatrix}
```

covariance  matrix：

$$
\widehat{\text{Cov}}(b_1, b_2, b_3) =
\begin{bmatrix}
3 & -2 & 1 \\
-2 & 4 & 0 \\
1 & 0 & 3
\end{bmatrix}
$$

樣本數為 63，我們接下來對三個虛無假設進行檢定，使用 5% 顯著水準。


## (a) \( H0: β2 = 0 \)

- Estimate: \( b_2 = 3 \)
- Variance: \( \text{Var}(b_2) = 4 \)
- Test statistic:


$$
t = \frac{3 - 0}{\sqrt{4}} = \frac{3}{2} = 1.5
$$

- Critical value (two-tailed): \( t_{0.025, 60} \approx 2.000 \)

**Conclusion**:  
Since \( |t| = 1.5 < 2.000 \), we **fail to reject** \( H_0 \)

---

## (b) \( H0: β1 + 2β2 = 5 \)

Let:

- \( R = \begin{bmatrix} 1 & 2 & 0 \end{bmatrix} \), \( r = 5 \)
- Estimated linear combination:

$$
Rb = 1(2) + 2(3) = 8
$$

- Variance of linear combination:

$$
\text{Var}(Rb) = R \cdot \text{Cov}(b) \cdot R^\top = 11
$$

- Test statistic:

$$
t = \frac{8 - 5}{\sqrt{11}} = \frac{3}{\sqrt{11}} \approx 0.9045
$$

- Critical value: \( t_{0.025, 60} \approx 2.000 \)

**Conclusion**:  
Since \( |t| \approx 0.9045 < 2.000 \), we **fail to reject** \( H_0 \)

---

## (c) \( H0: β1 - β2 + β3 = 4 \)

Let:

- \( R = \begin{bmatrix} 1 & -1 & 1 \end{bmatrix} \), \( r = 4 \)
- Estimated linear combination:

$$
Rb = 2 - 3 - 1 = -2
$$

- Variance of linear combination:

$$
\text{Var}(Rb) = R \cdot \text{Cov}(b) \cdot R^\top = 16
$$

- Test statistic:

$$
t = \frac{-2 - 4}{\sqrt{16}} = \frac{-6}{4} = -1.5
$$

- Critical value: \( t_{0.025, 60} \approx 2.000 \)

**Conclusion**:  
Since \( |t| = 1.5 < 2.000 \), we **fail to reject** \( H_0 \)

---

## Summary Table

| Hypothesis                                | Test Statistic | Critical Value | Conclusion               |
|-------------------------------------------|----------------|----------------|--------------------------|
| \( \β2 = 0 \)                             | 1.5            | 2.000          | Fail to reject \( H_0 \) |
| \( \β1 + 2β = 5 \)                        | ≈ 0.9045       | 2.000          | Fail to reject \( H_0 \) |
| \( \β1 - β2 + β3 = 4 \)                   | -1.5           | 2.000          | Fail to reject \( H_0 \) |

> **Note**: All tests are two-tailed with 5% significance level and 60 degrees of freedom.



--------------------------------------------------------------


## Q.31

<img src="https://github.com/user-attachments/assets/80ed1460-637b-49d0-b011-073641d83a50" alt="圖片描述" width="600" height="320" />



## Ans:
**(a)**

Null and Alternative Hypotheses

Null Hypothesis (H₀): β₂ = 0 (GDPB has no effect on MEDALS)

Alternative Hypothesis (H₁): β₂ > 0 (GDPB has a positive effect on MEDALS)

--------------------------------------------------------------

**(b)**






## Q.33

<img src="https://github.com/user-attachments/assets/cd21e3d8-c305-4f08-b41d-3968e2498319" alt="圖片描述" width="600" height="320" />



## Ans:
**(a)**

Null and Alternative Hypotheses

Null Hypothesis (H₀): β₂ = 0 (GDPB has no effect on MEDALS)

Alternative Hypothesis (H₁): β₂ > 0 (GDPB has a positive effect on MEDALS)

--------------------------------------------------------------

**(b)**


