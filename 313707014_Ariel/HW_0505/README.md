### f.

<img width="798" alt="截圖 2025-05-11 晚上9 14 54" src="https://github.com/user-attachments/assets/ffc57e0f-537f-4bc2-bd02-12f0f75d393c" />

Two equations derived from the moment conditions:

$$
\frac{1}{N} \sum x_{i1}(y_2 - \pi_1 x_{i1} - \pi_2 x_{i2}) = 0
$$

$$
\frac{1}{N} \sum x_{i2}(y_2 - \pi_1 x_{i1} - \pi_2 x_{i2}) = 0
$$


The equation can be simplified, and by plugging in the numbers, we find $$\hat{\pi}_1$$ = 3.

$$
\sum x_{i1} y_2 - \pi_1 \sum x_{i1}^2 - \pi_2 \sum x_{i1} x_{i2} = 0
$$

$$
3 - \pi_1 \cdot 1 - \pi_2 \cdot 0 = 0
$$

$$
\hat{\pi}_1 = 3
$$

Apply the same steps for the second equation, and we find $$\hat{\pi}_2$$ = 4.

$$
\sum x_{i2} y_2 - \pi_1 \sum x_{i2} x_{i1} - \pi_2 \sum x_{i2}^2 = 0
$$

$$
4 - 3 \cdot 0 - \pi_2 \cdot 1 = 0
$$

$$
\hat{\pi}_2 = 4
$$


---

### g.
<img width="798" alt="截圖 2025-05-11 晚上9 15 03" src="https://github.com/user-attachments/assets/ea1d9069-17a4-4d12-bac9-917e2ed3fe02" />

$$
y_1 = \alpha_1 y_2 + e_1
$$

Since $$y_2$$ is endogenous, we cannot use OLS directly. Thus, we use  $$\hat{y}_2$$ as an instrumental variables (IV) and $$\hat{y}_2$$ is uncorrelated with the error.  And according to moment condition for IV estimation and plugging in values, we could get the answer of $$\hat{\alpha_1}$$ is 18/25

$$
\hat{y}_2 = \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2
$$  


$$
E[(\hat{\pi}_1 x_1 + \hat{\pi}_2 x_2)(y_1- \alpha_1 y_2 ) \mid x] = 0
$$

$$
\frac{1}{N} \sum \begin{pmatrix} \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2 \end{pmatrix} (y_1- \alpha_1 y_2 ) = 0
$$

$$
\Rightarrow \sum (\hat{y}_2) (y_1- \alpha_1 y_2 ) = 0
$$

$$
\Rightarrow \sum \hat{y}_2 y_1 - \alpha_1 \sum \hat{y}_2 y_2 = 0
$$

$$
\Rightarrow \hat{\alpha_1} = \frac{\sum \hat{y}_2 y_1}{\sum\hat{y}_2 y_2}
$$

$$
\Rightarrow \frac{\sum y_1 (\hat{\pi}_1 x_1 + \hat{\pi}_2 x_2)}{\sum y_2 (\hat{\pi}_1 x_1 + \hat{\pi}_2 x_2)}
$$

$$
\Rightarrow \frac{\hat{\pi}_1 \sum y_1 x_1 + \hat{\pi}_2 \sum y_1 x_2}{ \hat{\pi}_1 \sum y_2 x_1 + \hat{\pi}_2 \sum y_2 x_2}
$$

plug in values ：

$$
= \frac{3 \times 2 + 4 \times 3}{3 \times 3 + 4 \times 4} = \frac{18}{25}
$$

---

### h. 
<img width="798" alt="截圖 2025-05-11 晚上9 15 09" src="https://github.com/user-attachments/assets/b69233ae-f00a-4a55-bb11-3515b8ff2232" />

$$
\hat{\alpha_1} = \frac{\sum \hat{y}_2 y_1}{\sum \hat{y}_2 y_2} 
= \frac{\sum \hat{y}_2 y_1}{\sum \hat{y}_2^2}
$$




According to moment condition:

$$
\hat{y}_2 = y_2 - v_i \Rightarrow y_2 = \hat{y}_2 + v_2
$$

$$
\sum \hat{y}_2 y_2 = \sum \hat{y}_2 (\hat{y}_2 + v_2) 
= \sum \hat{y}_2^2 + \sum \hat{y}_2 v_2
$$


Because $\hat{y}_2$ is uncorrelated with $v_2$, we have

$$
\sum \hat{y}_2 v_2 = 0
$$


$$
\sum \hat{y}_2^2 + \sum \hat{y}_2 v_2 = \sum \hat{y}_2^2 + 0
$$

Therefore we can prove that the IV estimator is  equal to the OLS estimator in this second-stage regression.



