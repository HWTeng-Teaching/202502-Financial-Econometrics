## Special Question: Q.01

## Let K=2, show that (b1, b2) in p. 29 of slides in Ch 5 reduces to the formula of (b1, b2) in (2.7) – (2.8)

#### CH05 P.29

![image](https://github.com/user-attachments/assets/fcf94938-b886-449a-97dd-f1cfc6974630)


#### CH02 2.7-2.8

![image](https://github.com/user-attachments/assets/bddf48e5-f129-43ad-8ea5-0d31c0e13b46)


## Ans:

1. 設定 $X$, $Y$

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

2. 代入公式 $\mathbf{b} = (X'X)^{-1}X'Y$

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

3. $\mathbf{b} = (X'X)^{-1}X'Y$

$$
(X'X)^{-1} = 
\frac{1}{n \sum x_i^2 - (\sum x_i)^2}
\begin{bmatrix}
\sum x_i^2 & -\sum x_i \\
-\sum x_i & n
\end{bmatrix}
$$

---

4. 乘上 $X'Y$

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

5. 展開後：

計算斜率項 $b_2$

$$
b_2 = \frac{-\sum x_i \sum y_i + n \sum x_i y_i}{n \sum x_i^2 - (\sum x_i)^2}
$$

代入 $\bar{x} = \frac{1}{n} \sum x_i$, $\bar{y} = \frac{1}{n} \sum y_i$

$$
= \frac{-n \bar{x} \sum y_i + n \sum x_i y_i}{n \sum x_i^2 - n^2 \bar{x}^2}
= \frac{n \sum x_i y_i - n^2 \bar{x} \bar{y}}{n \sum x_i^2 - n^2 \bar{x}^2}
$$

$$
= \frac{\sum x_i y_i - n \bar{x} \bar{y}}{\sum x_i^2 - n \bar{x}^2}
= \frac{\sum (x_i - \bar{x})(y_i - \bar{y})}{\sum (x_i - \bar{x})^2} \tag{2.7}
$$

---

計算截距項 $b_1$

$$
\begin{aligned}
b_1 &= \frac{\sum x_i^2 \sum y_i - \sum x_i \sum x_i y_i}{n \sum x_i^2 - (\sum x_i)^2} \\
    &= \frac{n \bar{y} \sum x_i^2 - \sum x_i \sum x_i y_i}{n \sum x_i^2 - (\sum x_i)^2} \\
    &= \bar{y} - b_2 \bar{x}
\end{aligned}
\tag{2.8}
$$



--------------------------------------------------------------


## Special Question: Q.02

## Let K=2, show that cov(b1, b2) in p. 30 of slides in Ch 5 reduces to the formula of in (2.14) – (2.16).

#### CH05 P.30

![image](https://github.com/user-attachments/assets/d85b12e6-7e20-4cf6-8d94-86888eb810d3)

#### CH02 2.14-2.16

![image](https://github.com/user-attachments/assets/b9d74ece-7cb4-431e-9ac4-cc355265afc4)


## Ans:
 
## 1. 矩陣形式：

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

## 2. 二乘反矩陣公式：

$$
(X'X)^{-1} = \frac{1}{n \sum x_i^2 - (\sum x_i)^2}
\begin{bmatrix}
\sum x_i^2 & -\sum x_i \\
-\sum x_i & n
\end{bmatrix}
$$

---

## 3. 乘上 $\sigma^2$：

$$
\mathrm{Var}(b) = \sigma^2 \cdot \frac{1}{n \sum x_i^2 - (\sum x_i)^2}
\begin{bmatrix}
\sum x_i^2 & -\sum x_i \\
-\sum x_i & n
\end{bmatrix}
$$

---

## 4. 展開矩陣各元素：

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

![image](https://github.com/user-attachments/assets/24c712c3-eaf5-4827-8c24-ecec200e389b)




## C5 Q.03

![image](https://github.com/user-attachments/assets/76814a2d-5147-4b9b-aadc-b06d9c0ee5a5)


## ANS:

**(a)**

(i)  

$$
t = \frac{\text{Coefficient}}{\text{Std. Error}} = \frac{1.4515}{2.2019} \approx 0.6593
$$


(ii) 

$$
t = \frac{\text{Coefficient}}{\text{Std. Error}} \Rightarrow \text{Std. Error} = \frac{2.7648}{5.7103} \approx 0.4840
$$


(iii) Coefficient=t×Std. Error=−3.9376×0.3695≈−1.4547

(iv) R²: To compute  R2 , we need SSE and SST. From the output,  SSE=5.752896  . 

To find  SST, we use the result

$$
s_y = \sqrt{\frac{SST}{N - 1}} = 6.39547
$$

which gives  SST= 1199 (6.39547)2= 49041.5   , Thus,

$$
R^2 = 1 - \frac{SSE}{SST} = 1 - \frac{46221.6}{49041.5} = 0.0575
$$


(v) σ̂ (Standard error of regression): 

$$
\hat{\sigma} = \sqrt{\frac{SSE}{N - K}} = \sqrt{\frac{46221.62}{1200 - 4}} = 6.217
$$



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

$$
t = \frac{-1.4547 - (-2)}{0.3695} \approx \frac{0.5453}{0.3695} \approx 1.476
$$

Critical value at 5% significance ≈ ±1.96

Conclusion: Fail to reject H₀. Insufficient evidence to show β₃ ≠ –2.

查 𝑑𝑓=1196（大樣本），臨界值為 ±1.96。因為 1.476 < 1.96，無法拒絕 𝐻0

結論： 在 5% 顯著水準下，資料不足以證明下降幅度不同於 2%。


--------------------------------------------------------------


## C5 Q.23

![image](https://github.com/user-attachments/assets/0b70c27c-9e3b-42b7-959e-60bdb411b6a7)


## Ans:

**(a)**

β₂ (QUANT): Expected to be negative. Larger transaction volumes are typically associated with lower per-unit prices due to quantity discounts.

β₃ (QUAL): Expected to be positive. Higher purity is assumed to command a higher price, reflecting a quality premium.

β₄ (TREND): Expected to be negative. Over time, prices may decline due to increasing supply, law enforcement pressure, or market maturity.

--------------------------------------------------------------

**(b)**

![image](https://github.com/user-attachments/assets/f66d5f26-01a0-4fa6-a353-c9d5c30eb457)


PRICE=90.8467−0.0600⋅QUANT+0.1162⋅QUAL−2.3546⋅TREND

![image](https://github.com/user-attachments/assets/af9f5fb8-e8ff-41e6-9f73-3cd73d92872c)


Yes — QUANT is negative (quantity discount), QUAL is positive (quality premium), TREND is negative (suggests decreasing prices).

--------------------------------------------------------------

**(c)**

R² = 0.5097

This means about 51% of the variation in price is explained by the model (QUANT, QUAL, and TREND).

This suggests moderate explanatory power.。


--------------------------------------------------------------

**(d)**

**Set up the hypotheses:**
- H₀: β₂ ≥ 0 (no effect of quantity on price)
- H₁: β₂ < 0 (quantity has a negative effect on price)
Result：
t value = -5.892 , one-tailed p value = 1.425e-07 

*For this hypothesis, The calculated t-value is -5.892. We reject H0.(less than or equal to the critical 
(t-=1.675, 0.95,n-4). To conclude that sellers are willing to accept a lower price if they can make sales in larger quantities.
 

--------------------------------------------------------------

**(e)**

H₀: β₃ = 0 (purity has no effect on price)

H₁: β₃ > 0 (higher purity increases price)

From output:

β₃ = 0.1162, t = 0.572, p = 0.5700

Conclusion: Fail to reject H₀.

There is no significant evidence that cocaine purity affects price in this dataset.  

--------------------------------------------------------------

**(f)**

The TREND coefficient is −2.3546, meaning the average price decreases by $2.35 per year.

Possible explanations:

Increased supply over time, outpacing demand.

Higher law enforcement risk, pushing prices down.

Market maturation, leading to price normalization.

 

![image](https://github.com/user-attachments/assets/8c095558-e03b-401a-a57d-6539e68f036c)


