## Special Question: Q.01

## Let K=2, show that (b1, b2) in p. 29 of slides in Ch 5 reduces to the formula of (b1, b2) in (2.7) – (2.8)

#### CH05 P.29

![image](https://github.com/user-attachments/assets/fcf94938-b886-449a-97dd-f1cfc6974630)

#### CH02 2.7-2.8

![image](https://github.com/user-attachments/assets/bddf48e5-f129-43ad-8ea5-0d31c0e13b46)


## Ans:

Given the vector equation $$Y = X\beta + e,$$ where $Y$ is a $N \times 1$ vector, $\beta$ is a $K \times 1$ vector, $e$ is a $N \times 1$ vector of errors, and $X$ is a $N \times K$ matrix of explanatory variables. The matrix $X$ includes a column of ones for the intercept, and considering $K = 2$, it implies that we have one intercept and one other variable. Thus, the matrix $X$ can be expressed as:

$$
X = \begin{pmatrix}
1 & x_{1,2} \\
1 & x_{2,2} \\
\vdots & \vdots \\
1 & x_{N,2}
\end{pmatrix}
$$

To compute the explicit formula for $(X'X)^{-1}$, we first calculate $X'X$. The transpose of $X$ is:

$$
X' = \begin{pmatrix}
1 & 1 & \cdots & 1 \\
x_{1,2} & x_{2,2} & \cdots & x_{N,2}
\end{pmatrix}
$$

Multiplying $X'$ and $X$ results in:

$$
X'X = \begin{pmatrix}
1 & 1 & \cdots & 1 \\
x_{1,2} & x_{2,2} & \cdots & x_{N,2}
\end{pmatrix}
\begin{pmatrix}
1 & x_{1,2} \\
1 & x_{2,2} \\
\vdots & \vdots \\
1 & x_{N,2}
\end{pmatrix} 
= \begin{pmatrix}
N & \sum_{i=1}^{N} x_{i,2} \\
\sum_{i=1}^{N} x_{i,2} & \sum_{i=1}^{N} x_{i,2}^2
\end{pmatrix}
$$

To find the inverse of a $2 \times 2$ matrix, 

$$
A = \begin{pmatrix} a & b \\
c & d
\end{pmatrix}
$$

the inverse is given by:

$$
A^{-1} = \frac{1}{ad-bc} \begin{pmatrix} d & -b \\
-c & a
\end{pmatrix}
$$

Applying this to our matrix $X'X$, where $a = N$, $b = c = \sum_{i=1}^{N} (x_{i,2})$, and $d = \sum_{i=1}^{N} (x_{i,2})^2$, we have:

$$
(X'X)^{-1} = \frac{1}{N \sum_{i=1}^{N} x_{i,2}^2 - (\sum_{i=1}^{N} x_{i,2})^2} \begin{pmatrix}
\sum_{i=1}^{N} x_{i,2}^2 & -\sum_{i=1}^{N} x_{i,2} \\
-\sum_{i=1}^{N} x_{i,2} & N
\end{pmatrix}
$$

This formula gives us the inverse of $X'X$, which is crucial in the calculation of the estimator for $\beta$ in the ordinary least squares regression model:

$$
b = (X'X)^{-1}(X'Y)
$$

This completes the derivation for the explicit formula of $(X'X)^{-1}$ in the case when $K = 2$.


#### Derive $b=(X'X)^{-1}(X'Y)$ element by element for $K=2$.

To derive the elements of $b = (X'X)^{-1}(X'Y)$ for $K = 2$, we first need the individual elements of $(X'X)^{-1}$ and $X'Y$.

### 1. Calculate $X'X$ and $(X'X)^{-1}$

As previously derived, for $K = 2$ (one intercept and one regressor):

$$
X'X = \begin{pmatrix}
N & \sum x_{i,2} \\
\sum x_{i,2} & \sum x_{i,2}^2
\end{pmatrix}
$$

The inverse, $(X'X)^{-1}$, is given by:

$$
(X'X)^{-1} = \frac{1}{\Delta} \begin{pmatrix}
\sum x_{i,2}^2 & -\sum x_{i,2} \\
-\sum x_{i,2} & N
\end{pmatrix}
$$

where $\Delta = N \sum x_{i,2}^2 - (\sum x_{i,2})^2$.

### 2. Calculate $X'Y$

$$
X'Y = \begin{pmatrix}
1 & 1 & \cdots & 1 \\
x_{1,2} & x_{2,2} & \cdots & x_{N,2}
\end{pmatrix}
\begin{pmatrix}
y_1 \\
y_2 \\
\vdots \\
y_N
\end{pmatrix}
= \begin{pmatrix}
\sum y_i \\
\sum x_{i,2} y_i
\end{pmatrix}
$$

### 3. Calculate $b = (X'X)^{-1}(X'Y)$

We now compute the product of $(X'X)^{-1}$ and $X'Y$:

$$
b = \frac{1}{\Delta} \begin{pmatrix}
\sum x_{i,2}^2 & -\sum x_{i,2} \\
-\sum x_{i,2} & N
\end{pmatrix}
\begin{pmatrix}
\sum y_i \\
\sum x_{i,2} y_i
\end{pmatrix}
$$

Let's explicitly calculate the elements of $b$:

**Element $b_1$** (intercept):

$$
b_1 = \frac{1}{\Delta} \left( \left(\sum x_{i,2}^2\right) \left(\sum y_i\right) - \left(\sum x_{i,2}\right) \left(\sum x_{i,2} y_i\right) \right)
$$

**Element $b_2$** (slope):

$$
b_2 = \frac{1}{\Delta} \left( -\left(\sum x_{i,2}\right) \left(\sum y_i\right) + N \left(\sum x_{i,2} y_i\right) \right)
$$

This gives us the two elements of $b$, where $b_1$ is the estimated intercept and $b_2$ is the estimated slope of the regression line. Each element is calculated by the respective formula, reflecting how each factor (intercept and slope) is influenced by the sums of $y_i$, $x_{i,2}$, and their products, scaled by the determinant $\Delta$ of the matrix $(X'X)^{-1}$.


--------------------------------------------------------------


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



## C5 Q.03

![image](https://github.com/user-attachments/assets/76814a2d-5147-4b9b-aadc-b06d9c0ee5a5)


## ANS:

**(a)**

(i)  ![image](https://github.com/user-attachments/assets/cc406472-4424-44fc-bc8c-4696c71e9a6b)


(ii) ![image](https://github.com/user-attachments/assets/ca8273a5-c8a8-4abc-a99a-3035e6ba94c7)


(iii) Coefficient=t×Std. Error=−3.9376×0.3695≈−1.4547

(iv) R²: To compute  R2 , we need SSE and SST. From the output,  SSE=5.752896  . 

To find  SST, we use the result

![image](https://github.com/user-attachments/assets/cfeec6e9-ad55-4d8b-884a-239abd32e5d3)

which gives  SST= 1199 (6.39547)2= 49041.5   , Thus,

![image](https://github.com/user-attachments/assets/b1a8d964-2e5c-4998-abd7-853fa560097c)


(v) σ̂ (Standard error of regression): 

![image](https://github.com/user-attachments/assets/a294ffbf-07b8-43f2-84c5-552aef2aa369)



--------------------------------------------------------------

**(b)**

β₂ = 2.7648: A 1% increase in total expenditure increases alcohol budget share by 2.76 percentage points.

β₃ = –1.4547: Each additional child reduces alcohol budget share by 1.45 percentage points.

β₄ = –0.1503: Each additional year in household head age reduces alcohol budget share by 0.15 percentage points.


--------------------------------------------------------------

**(c)**

β 4±z 0.025⋅SE=−0.1503±1.96×0.0235=−0.1503±0.0461⇒[−0.1964,−0.1042]

Interval: [–0.1964, –0.1042]

Interpretation: With 95% confidence, a one-year increase in age reduces alcohol budget share between 0.1042 and 0.1964 percentage points.


--------------------------------------------------------------

**(d)**

β₁ (p=0.5099): Not significant

β₂, β₃, β₄ (p < 0.05): Statistically significant


![image](https://github.com/user-attachments/assets/0e636bc1-1975-4dee-91af-39f87a65d0dd)


--------------------------------------------------------------

**(e)**

𝐻0:𝛽3=−2

𝐻1:𝛽3≠−2

![image](https://github.com/user-attachments/assets/443c11f4-7031-42e4-9945-8d51f8dc742c)

Critical value at 5% significance ≈ ±1.96

Conclusion: Fail to reject H₀. Insufficient evidence to show β₃ ≠ –2.

查 𝑑𝑓=1196（大樣本），臨界值為 ±1.96。因為 1.476 < 1.96，無法拒絕 𝐻0

結論： 在 5% 顯著水準下，資料不足以證明下降幅度不同於 2%。


--------------------------------------------------------------

**(f)**

β₄ = –0.1503: Each year, alcohol budget share decreases by ~0.15 percentage points.

Possible reasons: health awareness, regulatory changes, cultural shifts.


--------------------------------------------------------------


## C5 Q.23

![image](https://github.com/user-attachments/assets/0b70c27c-9e3b-42b7-959e-60bdb411b6a7)


## Ans:

**(a)**

β₂ (QUANT): Negative, due to quantity discount theory (larger quantity = lower price per gram).

β₃ (QUAL): Positive, higher purity should command a premium.

β₄ (TREND): Could be positive or negative depending on market trends (e.g., enforcement, demand, inflation).

--------------------------------------------------------------

**(b)**

lm(formula = price ~ quant + qual + trend, data = cocaine)

Residuals:
    Min      1Q  Median      3Q     Max 
-43.479 -12.014  -3.743  13.969  43.753 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 90.84669    8.58025  10.588 1.39e-14 ***
quant       -0.05997    0.01018  -5.892 2.85e-07 ***
qual         0.11621    0.20326   0.572   0.5700    
trend       -2.35458    1.38612  -1.699   0.0954 .  
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 20.06 on 52 degrees of freedom
Multiple R-squared:  0.5097,	Adjusted R-squared:  0.4814 
F-statistic: 18.02 on 3 and 52 DF,  p-value: 3.806e-08

The estimated equation is:

PRICE = 90.8467 - 0.0600 QUANT + 0.1162 QUAL - 2.3546TREND     R2= 0.5097

(se)    (8.5803)   (0.0102)       (0.2033)     (1.3861)        

(t )    (10.588)   ( 5.892)       (0.5717)    − ( 1.6987)  

The estimated values for B2 B3 B4 are -0.0600 , 0.1162 and -2.3546  , respectively. 
They imply that as quantity (number of grams in one sale) increases by 1 unit, the mean price will go down by 0.0600. Also, as the quality increases by 1 unit the mean price goes up by 0.1162. As time increases by 1 year, the mean price decreases by 2.3546. All the signs turn out according to our expectations, with β4 implying supply has been increasing faster than demand. 


--------------------------------------------------------------

**(c)**

The proportion of variation in cocaine price explained by the variation in quantity, quality and time is 0.5097.  

--------------------------------------------------------------

**(d)**

For this hypothesis we test H0: B2≧0 against : H1: β2 < 0 . The calculated t-value is 
-5.892  . We reject H0 if the calculated t is less than or equal to the critical 
t(0.95,52)= 1.675 . Since the calculated t is less than the critical t value, we reject H0 and conclude that sellers are willing to accept a lower price if they can make sales in larger quantities. 

--------------------------------------------------------------

**(e)**

We want to test H0:B3≦0  against H1: β>0 . The calculated t-value is 0.5717. At 
α= 0.05 we reject H0 if the calculated t is greater than or equal to 1.675. Since for this case, the calculated t is not greater than the critical t, we do not reject H0 . We cannot conclude that a premium is paid for better quality cocaine.  

--------------------------------------------------------------

**(f)**

The average annual change in the cocaine price is given by the value of B4=-2.3546 . It has a negative sign suggesting that the price decreases over time. A possible reason for a decreasing price is the development of improved technology for producing cocaine, such that suppliers can produce more at the same cost. 


![image](https://github.com/user-attachments/assets/8c095558-e03b-401a-a57d-6539e68f036c)


