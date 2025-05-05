# Exercise 11.1 

Our aim is to estimate the parameters of the simultaneous equations model:

$$
y_1 = \alpha_1 y_2 + e_1 \\
y_2 = \alpha_2 y_1 + \beta_1 x_1 + \beta_2 x_2 + e_2
$$

We assume that \( x_1 \) and \( x_2 \) are exogenous and uncorrelated with the error terms \( e_1 \) and \( e_2 \).

### (a)
Solve the two structural equations for the reduced-form equation for \( y_2 \), that is,  
\( y_2 = \pi_1 x_1 + \pi_2 x_2 + \nu_2 \).  
Express the reduced-form parameters in terms of the **structural parameters**, and the reduced-form error in terms of \( e_1 \) and \( e_2 \).  
Show that \( y_2 \) is correlated with \( e_1 \).

### (b)
Which equation parameters are consistently estimated using OLS? Explain.

### (c)
Which parameters are "identified" in the simultaneous equations sense? Explain your reasoning.

### (d)
To estimate the parameters of the reduced-form equation for \( y_2 \) using the **method of moments (MOM)**, the two moment equations are:

$$
\frac{1}{N} \sum x_{1i}(y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0 \\
\frac{1}{N} \sum x_{2i}(y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0
$$

Explain why these two moment conditions are a valid basis for obtaining consistent estimators of the reduced-form parameters.

### (e)
Are the MOM estimators in part (d) the same as the OLS estimators?  
Form the sum of squared errors for \( y_2 = \pi_1 x_1 + \pi_2 x_2 + \nu_2 \), take derivatives, and show they yield the same equations.

### (f)
Given:
- \( \sum x_{1i}^2 = 1 \)
- \( \sum x_{2i}^2 = 1 \)
- \( \sum x_{1i} x_{2i} = 0 \)
- \( \sum x_{1i} y_{i} = 2 \)
- \( \sum x_{1i} y_{2i} = 3 \)
- \( \sum x_{2i} y_{i} = 3 \)
- \( \sum x_{2i} y_{2i} = 4 \)

Use the moment conditions from part (d) to solve for \( \hat{\pi}_1 = 3 \), \( \hat{\pi}_2 = 4 \).

### (g)
The fitted value is:
$$
\hat{y}_2 = \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2
$$

Explain why we can use the moment condition:
$$
\sum x_{2i} (\hat{y}_{1i} - \alpha_1 y_{2i}) = 0
$$
to consistently estimate \( \alpha_1 \), and compute the IV estimate.

### (h)
Find the 2SLS estimate of \( \alpha_1 \) by applying OLS to:
$$
y_1 = \alpha_1 \hat{y}_2 + e_1^*
$$
Compare with the answer in (g).

---

## 中文翻譯

我們的目標是估計下列聯立方程模型中的參數：

$$
y_1 = \alpha_1 y_2 + e_1 \\
y_2 = \alpha_2 y_1 + \beta_1 x_1 + \beta_2 x_2 + e_2
$$

我們假設 \( x_1 \)、\( x_2 \) 是外生變數，與誤差項 \( e_1 \)、\( e_2 \) 無相關。

### （a）
求解 \( y_2 \) 的簡約式方程：
$$
y_2 = \pi_1 x_1 + \pi_2 x_2 + \nu_2
$$
請將簡約式係數表示為結構參數的函數，並將誤差項表示為 \( e_1 \)、\( e_2 \) 的函數。請證明 \( y_2 \) 與 \( e_1 \) 有相關性。

### （b）
哪個方程中的參數可以用 OLS 一致估計？請說明理由。

### （c）
根據聯立方程中的識別性定義，哪些參數是可識別的？請說明理由。

### （d）
欲使用「矩估法（MOM）」估計 \( y_2 \) 的簡約式參數，根據第 10.3 節，可得兩個矩條件如下：

$$
\frac{1}{N} \sum x_{1i}(y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0 \\
\frac{1}{N} \sum x_{2i}(y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0
$$

請解釋這兩個條件為何可以一致估計簡約式參數。

### （e）
第 (d) 小題中的 MOM 估計量是否與 OLS 相同？請對 \( y_2 = \pi_1 x_1 + \pi_2 x_2 + \nu_2 \) 進行最小平方法推導，證明導數設為零後等同於 MOM 矩條件。

### （f）
給定：
- \( \sum x_{1i}^2 = 1 \)
- \( \sum x_{2i}^2 = 1 \)
- \( \sum x_{1i} x_{2i} = 0 \)
- \( \sum x_{1i} y_i = 2 \)
- \( \sum x_{1i} y_{2i} = 3 \)
- \( \sum x_{2i} y_i = 3 \)
- \( \sum x_{2i} y_{2i} = 4 \)

請利用 (d) 的矩條件求得 \( \hat{\pi}_1 = 3 \)、\( \hat{\pi}_2 = 4 \)。

### （g）
令：
$$
\hat{y}_2 = \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2
$$

請說明為何以下矩條件可以一致估計 \( \alpha_1 \)：
$$
\sum x_{2i} (\hat{y}_{1i} - \alpha_1 y_{2i}) = 0
$$
並求出 IV 估計量 \( \alpha_1 \)。

### （h）
使用下列式子進行 OLS，找出 \( \alpha_1 \) 的 2SLS 估計值：
$$
y_1 = \alpha_1 \hat{y}_2 + e_1^*
$$
並與 (g) 小題結果比較。

