# Q1
![Q1](https://github.com/user-attachments/assets/fb8d4535-5e1a-41f4-8dc9-f62cbfcbae6b)



### 1. set up  $X$, $Y$

```math
X = 
\begin{bmatrix}
1 & x_1 \\
1 & x_2 \\
\vdots & \vdots \\
1 & x_n
\end{bmatrix}_{n \times 2}, \quad
Y = 
\begin{bmatrix}
y_1 \\
\vdots \\
y_n
\end{bmatrix}_{n \times 1}
```

---

### 2. Substitute into the formula $\mathbf{b} = (X'X)^{-1}X'Y$

```math
X'X = 
\begin{bmatrix}
1 & \cdots & 1 \\
x_1 & \cdots & x_n
\end{bmatrix}
\begin{bmatrix}
1 & x_1 \\
\vdots & \vdots \\
1 & x_n
\end{bmatrix}
=
\begin{bmatrix}
n & \sum x_i \\
\sum x_i & \sum x_i^2
\end{bmatrix}
```

```math
X'Y = 
\begin{bmatrix}
1 & \cdots & 1 \\
x_1 & \cdots & x_n
\end{bmatrix}
\begin{bmatrix}
y_1 \\
\vdots \\
y_n
\end{bmatrix}
=
\begin{bmatrix}
\sum y_i \\
\sum x_i y_i
\end{bmatrix}
```

---

### 3. $\mathbf{b} = (X'X)^{-1}X'Y$

$$
(X'X)^{-1} = 
\frac{1}{n \sum x_i^2 - (\sum x_i)^2}
\begin{bmatrix}
\sum x_i^2 & -\sum x_i \\
-\sum x_i & n
\end{bmatrix}
$$

---

### 4. Multiply by $X'Y$

$$
\mathbf{b} = (X'X)^{-1}X'Y =
\frac{1}{n \sum x_i^2 - (\sum x_i)^2}
\begin{bmatrix}
\sum x_i^2 & -\sum x_i \\
-\sum x_i & n
\end{bmatrix}
\begin{bmatrix}
\sum y_i \\
\sum x_i y_i
\end{bmatrix}
$$

---

### 5. After expansion

* Compute the slope term $b_2$

$$
b_2 = \frac{-\sum x_i \sum y_i + n \sum x_i y_i}{n \sum x_i^2 - (\sum x_i)^2}
$$

Substitute  $\bar{x} = \frac{1}{n} \sum x_i$, $\bar{y} = \frac{1}{n} \sum y_i$

$$
= \frac{-n \bar{x} \sum y_i + n \sum x_i y_i}{n \sum x_i^2 - n^2 \bar{x}^2}
= \frac{n \sum x_i y_i - n^2 \bar{x} \bar{y}}{n \sum x_i^2 - n^2 \bar{x}^2}
$$

$$
= \frac{\sum x_i y_i - n \bar{x} \bar{y}}{\sum x_i^2 - n \bar{x}^2}
= \frac{\sum (x_i - \bar{x})(y_i - \bar{y})}{\sum (x_i - \bar{x})^2} \tag{2.7}
$$

---

* Compute the intercept term $b_1$

$$
\begin{aligned}
b_1 &= \frac{\sum x_i^2 \sum y_i - \sum x_i \sum x_i y_i}{n \sum x_i^2 - (\sum x_i)^2} \\
    &= \frac{n \bar{y} \sum x_i^2 - \sum x_i \sum x_i y_i}{n \sum x_i^2 - (\sum x_i)^2} \\
    &= \bar{y} - b_2 \bar{x}
\end{aligned}
\tag{2.8}
$$
# Q2

![Q2](https://github.com/user-attachments/assets/3e486b1f-65f0-48b5-8756-bd4cefaaecca)

 
### 1. Matrix form:

$$
\mathrm{Var}(b) = \sigma^2 (X'X)^{-1}, \quad \text{when } K = 2
$$

$$
X =
\begin{bmatrix}
1 & x_1 \\
1 & x_2 \\
\vdots & \vdots \\
1 & x_n
\end{bmatrix}
\quad \Rightarrow \quad
X'X =
\begin{bmatrix}
n & \sum x_i \\
\sum x_i & \sum x_i^2
\end{bmatrix}
$$

---

### 2. Inverse formula for a 2×2 matrix:

$$
(X'X)^{-1} = \frac{1}{n \sum x_i^2 - (\sum x_i)^2}
\begin{bmatrix}
\sum x_i^2 & -\sum x_i \\
-\sum x_i & n
\end{bmatrix}
$$

---

### 3. Multiply by $\sigma^2$：

$$
\mathrm{Var}(b) = \sigma^2 \cdot \frac{1}{n \sum x_i^2 - (\sum x_i)^2}
\begin{bmatrix}
\sum x_i^2 & -\sum x_i \\
-\sum x_i & n
\end{bmatrix}
$$

---

### 4. Expand each element of the matrix:

$$
\begin{aligned}
\mathrm{Var}(b_1)
&= \sigma^2 \cdot \frac{\sum x_i^2}{n \sum x_i^2 - (\sum x_i)^2} \\
&= \sigma^2 \cdot \left[ \frac{\sum x_i^2}{n \sum (x_i - \bar{x})^2} \right]
\end{aligned}
\tag{2.14}
$$

---

$$
\begin{aligned}
\mathrm{Var}(b_2)
&= \sigma^2 \cdot \frac{n}{n \sum x_i^2 - (\sum x_i)^2} \\
&= \frac{\sigma^2}{\sum (x_i - \bar{x})^2}
\end{aligned}
\tag{2.15}
$$

---

$$
\begin{aligned}
\mathrm{Cov}(b_1, b_2)
&= \sigma^2 \cdot \frac{-\sum x_i}{n \sum x_i^2 - (\sum x_i)^2} \\
&= \sigma^2 \cdot \frac{-\bar{x}}{\sum (x_i - \bar{x})^2}
\end{aligned}
\tag{2.16}
$$

---

# 5.3
![image](https://github.com/user-attachments/assets/691d3294-4b74-41f0-97e6-11d0cda602c9)
![image](https://github.com/user-attachments/assets/0d7a8d11-757b-4495-8f88-bb5b722efda0)
## a.
i. t-statistic for β₁: 

$$
t = \frac{\text{Coefficient}}{\text{Std. Error}} = \frac{1.4515}{2.2019} \approx 0.6592
$$

ii. Standard error for β₂: 

$$
t = \frac{\text{Coefficient}}{\text{Std. Error}} \Rightarrow \text{Std. Error} = \frac{2.7648}{5.7103} \approx 0.4842
$$

iii. Estimate for β₃: 

 Coefficient = t × Std. Error = –3.9376 × 0.3695 ≈ –1.4549  
 
(iv)  R² :  To compute R² , we need SSE and SST. From the output,  SSE=5.752896  . 

To find  SST, we use the result

$$
s_y = \sqrt{\frac{SST}{N - 1}} = 6.39547
$$

which gives  SST= 1199 (6.39547)2= 49041.5   , Thus,

$$
R^2 = 1 - \frac{SSE}{SST} = 1 - \frac{46221.6}{49041.5} = 0.0575
$$

    
v. σ̂ (Standard error of regression): 

$$
\hat{\sigma} = \sqrt{\frac{SSE}{N - K}} = \sqrt{\frac{46221.62}{1200 - 4}} = 6.217
$$

## b.
 
β₂ = 2.7648:** A 1% increase in total expenditure increases alcohol budget share by 2.76 percentage points.   
β₃ = –1.454943:** Each additional child reduces alcohol budget share by 1.45 percentage points.   
β₄ = –0.1503:** Each additional year in household head age reduces alcohol budget share by 0.15 percentage points.   

## c.
$$
\beta_4 \pm z_{0.025} \cdot SE = -0.1503 \pm 1.96 \times 0.0235 = -0.1503 \pm 0.0461 \Rightarrow [-0.1964, -0.1042]
$$

**Interpretation:** With 95% confidence, a one-year increase in age reduces alcohol budget share between 0.1042 and 0.1964 percentage points.

## d.
With the exception of the intercept, all coefficient estimates are significantly different from zero at a 5% level because their p-values are all less than 0.05.   
![image](https://github.com/user-attachments/assets/4bb5ede9-58d5-400e-9ccd-26820ce4a8ec)

## e.
<img width="516" alt="image" src="https://github.com/user-attachments/assets/bd63f2fa-6d22-437f-8de9-37b3e7777b4d" />

------
# 5.23
**(a)** 
Q: What signs would you expect on the coefficients β₂, β₃, and β₄?

Ans:

Expected Signs of the Coefficients
β₂ (QUANT): Expected to be negative(-). Larger transaction volumes are typically associated with lower per-unit prices due to quantity discounts.
β₃ (QUAL): Expected to be positive(+). Higher purity is assumed to command a higher price, reflecting a quality premium.
β₄ (TREND): Expected to be negative(-). Over time, prices may decline due to increasing supply, law enforcement pressure, or market maturity.

---

**(b)** 
Q: Use your computer software to estimate the equation. Report the results and interpret the coefficient estimates. Have the signs turned out as you expected?

Ans:
### price = 90.85 -0.06 *quant + 0.12 *qual -2.35 *trend 

|              |   Estimate   |  Std. Error  |   t value   |      Pr [>abs(t)]      |
|--------------|--------------|--------------|-------------|--------------------|
| (Intercept)  | 90.84668753  | 8.58025368   | 10.5878790  | 1.393119e-14       |
| quant        | -0.05996979  | 0.01017828   | -5.8919359  | 2.850720e-07       |
| qual         | 0.11620520   | 0.20326448   | 0.5716946   | 5.699920e-01       |
| trend        | -2.35457895  | 1.38612032   | -1.6986829  | 9.535543e-02       |

**(R code)**

<img src="https://github.com/user-attachments/assets/9571d4f4-4dbd-4ebc-ac6f-2300f424d08c" alt="圖片描述" width="600" height="175" />

---

**(c)**
Q: What proportion of variation in cocaine price is explained jointly by variation in quantity, quality, and time?

Ans:
### Model R² = 0.5097 ~51%

This means about 51% of the variation in price is explained by the model (QUANT, QUAL, and TREND).

*[51%] This suggests moderate explanatory power

**(R code)**

<img src="https://github.com/user-attachments/assets/63983d8b-2754-49da-abbd-86e16943733f" alt="圖片描述" width="600" height="90" />

---

**(d)**
Q: It is claimed that the greater the number of sales, the higher the risk of getting caught. Thus, sellers are willing to accept a lower price if they can make sales in larger quantities. Set up H₀ and H₁ that would be appropriate to test this hypothesis. Carry out the hypothesis test.

Ans:

**Set up the hypotheses:**
- H₀: β₂ ≥ 0 (no effect of quantity on price)
- H₁: β₂ < 0 (quantity has a negative effect on price)
Result：
t value = -5.892 , one-tailed p value = 1.425e-07 

*For this hypothesis, The calculated t-value is -5.892. We reject H0.(less than or equal to the critical 
(t-=1.675, 0.95,n-4). To conclude that sellers are willing to accept a lower price if they can make sales in larger quantities.

<img src="https://github.com/user-attachments/assets/cff99f32-ee49-4454-a345-9f8a45ddaa4f" alt="圖片描述" width="350" height="150" />

**(R code)**

<img src="https://github.com/user-attachments/assets/9da8ffa3-2702-4541-83d9-88a405efba4c" alt="圖片描述" width="600" height="400" />

---

**(e)**
Q: Test the hypothesis that the quality of cocaine has no influence on expected price against the alternative that a premium is paid for better-quality cocaine.

Ans:

**Set up the Hypothesis:**
- H₀: β₃ ≤ 0 (no effect of quality on price)
- H₁: β₃ > 0 (higher quality leads to higher price)
Result：
t value = 0.572 , one-tailed p value = 0.284996

*The calculated t-value is 0.5717. At α=0.05, we can not reject H0 by the calculated t is less than or equal to 1.675. 

& We cannot conclude that a premium is paid for better quality cocaine. 

<img src="https://github.com/user-attachments/assets/50c3009c-cf9a-4519-be51-212a5270df0b" alt="圖片描述" width="350" height="150" />

**(R code)**

<img src="https://github.com/user-attachments/assets/bb19a18f-1417-4b7c-b822-e1e680b8757b" alt="圖片描述" width="600" height="400" />

---

**(f)**
Q: What is the average annual change in the cocaine price? Can you suggest why price might be changing in this direction?

Ans:

### average annual change in the cocaine price (β₄) = -2.35 USD per year

*The average annual change in the cocaine price is given by the value of -2.3546. It has a negative sign suggesting that the price decreases over time. A possible reason for a decreasing price is the development of improved technology for producing cocaine, such hat suppliers can produce more at the same cost.  

<img src="https://github.com/user-attachments/assets/b882fa00-495a-48c1-bb32-c92d10731991" alt="圖片描述" width="600" height="300" />

**(R code)**

<img src="https://github.com/user-attachments/assets/a970615b-53ff-452e-bc76-41909e90e347" alt="圖片描述" width="600" height="300" />

---

