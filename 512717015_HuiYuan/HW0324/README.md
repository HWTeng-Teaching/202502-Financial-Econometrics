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
