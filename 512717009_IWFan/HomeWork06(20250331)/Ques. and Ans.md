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
\widehat{\text{Cov}}(b1, b2, b3) =
\begin{bmatrix}
3 & -2 & 1 \\
-2 & 4 & 0 \\
1 & 0 & 3
\end{bmatrix}
$$

樣本數為 63，我們接下來對三個虛無假設進行檢定，使用 5% 顯著水準。



<img src="https://github.com/user-attachments/assets/bab10ef1-1830-4ba7-a870-ded971ec8896" alt="圖片描述" width="600" height="320" />



--------------------------------------------------------------



<img src="https://github.com/user-attachments/assets/9456a6fc-ddd8-4161-8e36-4ee29587cf5a" alt="圖片描述" width="600" height="600" />




--------------------------------------------------------------




<img src="https://github.com/user-attachments/assets/bbdffcb8-a4b7-460b-972b-3741754f8800" alt="圖片描述" width="900" height="700" />






## Summary Table

| Hypothesis                                | Test Statistic | Critical Value | Conclusion               |
|-------------------------------------------|----------------|----------------|--------------------------|
| \( β2 = 0 \)                              | 1.5            | 2.000          | Fail to reject \( H0 \)  |
| \( β1 + 2β = 5 \)                         | ≈ 0.9045       | 2.000          | Fail to reject \( H0 \)  |
| \( β1 - β2 + β3 = 4 )                     | -1.5           | 2.000          | Fail to reject \( H0 \)  |

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


