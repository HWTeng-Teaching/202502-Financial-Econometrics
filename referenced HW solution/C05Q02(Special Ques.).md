
*BY 范艾雯 Eva Fan (512717009)*

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

$$
\mathrm{Var}(\mathbf{b}) = \sigma^2 (X'X)^{-1} =
\sigma^2 \cdot \frac{1}{n \sum x_i^2 - (\sum x_i)^2}
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


![image](https://github.com/user-attachments/assets/24c712c3-eaf5-4827-8c24-ecec200e389b)


