# 11.1
![image](https://github.com/user-attachments/assets/d34d18fd-da8a-476f-853d-fc52bd5f4a6e)

## **(a) 消去與代入法：推導 reduced form**

方程組：

$$
y_1 = \alpha_1 y_2 + e_1 \\
$$

$$
y_2 = \alpha_2 y_1 + \beta_1 x_1 + \beta_2 x_2 + e_2
$$

將方程式 (1) 代入方程式 (2) 並化簡：

$$
\begin{aligned}
y_2 &= \alpha_2 y_1 + \beta_1 x_1 + \beta_2 x_2 + e_2 \\
&= \alpha_2 (\alpha_1 y_2 + e_1) + \beta_1 x_1 + \beta_2 x_2 + e_2 \\
&= \alpha_1 \alpha_2 y_2 + \alpha_2 e_1 + \beta_1 x_1 + \beta_2 x_2 + e_2
\end{aligned}
$$

將 y_2  移至左邊：

$$
y_2 (1 - \alpha_1 \alpha_2) = \beta_1 x_1 + \beta_2 x_2 + e_2 + \alpha_2 e_1
$$

Solve for \( y_2 \):

$$
y_2 = \frac{\beta_1}{1 - \alpha_1 \alpha_2} x_1 + \frac{\beta_2}{1 - \alpha_1 \alpha_2} x_2 + \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2}
$$

定義：

$$
\pi_1 = \frac{\beta_1}{1 - \alpha_1 \alpha_2}, \quad 
\pi_2 = \frac{\beta_2}{1 - \alpha_1 \alpha_2}, \quad 
v_2 = \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2}
$$

則：

$$
y_2 = \pi_1 x_1 + \pi_2 x_2 + v_2
$$

To show the correlation:

$$
\begin{aligned}
\text{cov}(y_2, e_1 \mid \mathbf{x}) 
&= E(y_2 e_1 \mid \mathbf{x}) \\
&= E \left[ \left( \frac{\beta_1}{1 - \alpha_1 \alpha_2} x_1 + \frac{\beta_2}{1 - \alpha_1 \alpha_2} x_2 + \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2} \right) e_1 \,\middle|\, \mathbf{x} \right] \\
&= E \left[ \frac{\beta_1}{1 - \alpha_1 \alpha_2} x_1 e_1 \mid \mathbf{x} \right] + E \left[ \frac{\beta_2}{1 - \alpha_1 \alpha_2} x_2 e_1 \mid \mathbf{x} \right] + E \left[ \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2} e_1 \mid \mathbf{x} \right] \\
&= 0 + 0 + E \left[ \frac{e_2 e_1 + \alpha_2 e_1^2}{1 - \alpha_1 \alpha_2} \mid \mathbf{x} \right] \\
&= \frac{\alpha_2 \sigma_{e_1}^2}{1 - \alpha_1 \alpha_2} \quad \text{(若 } e_1 \text{ 與 } e_2 \text{ 不相關)}
\end{aligned}
$$

因此，當 α2 ≠ 0  且 α1 ≠ 0  時， y2  與 e1  相關 ⇒ 說明內生性。

我們已得：

$$
\text{cov}(y_2, e_1 \mid \mathbf{x}) = E(y_2 e_1 \mid \mathbf{x})
$$

接著，

$$
\begin{aligned}
\text{cov}(y_2, e_1 \mid \mathbf{x}) 
&= \frac{E(e_2 e_1 \mid \mathbf{x}) + \alpha_2 E(e_1^2 \mid \mathbf{x})}{1 - \alpha_1 \alpha_2} \\
&= \frac{\alpha_2}{1 - \alpha_1 \alpha_2} \sigma_1^2
\end{aligned}
$$

此共變數非零，除非：

$$
\alpha_2 = 0 \Rightarrow \text{無同時性問題 (no simultaneity)}
$$

## **(b) OLS 可一致估計的條件**

因為原始兩個結構式的右邊都包含內生變數（如  y_1 ），所以不能直接使用 OLS。

但 reduced form 中只有外生變數  x_1, x_2 ，可以對其進行 OLS 一致估計。

## **(c) 識別性（Identification）**

結構模型中  M = 2  個方程式。為了識別一條方程式，必須省略  M - 1 = 1  個外生變數。

- 方程 (1) 中已省略  x_1, x_2 ，是 **已識別**
- 方程 (2) 中未省略任何外生變數，**未被識別**

## **(d) Moment Conditions**

這些 moment conditions 來自於  x  為外生變數（exogenous）的假設。因此有：

$$
E(x_{i1} v_{i1} \mid \mathbf{x}) = E(x_{i2} v_{i2} \mid \mathbf{x}) = 0
$$

根據 part (a) 的結果， y_2  的 reduced form 為：

$$
y_2 = \frac{\beta_1}{1 - \alpha_1 \alpha_2} x_1 + \frac{\beta_2}{1 - \alpha_1 \alpha_2} x_2 + \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2}
$$

簡記為：

$$
y_2 = \pi_1 x_1 + \pi_2 x_2 + v_2
$$

Reduced Form 誤差項與  x  無相關性證明：

我們要證明：

<img width="495" alt="d" src="https://github.com/user-attachments/assets/8fd9f1e9-f1b4-44f2-a14f-8e530680cf4f" />


這是因為  x  為外生變數，對  e_1, e_2  條件期望為 0。

因此，reduced form 的誤差項  v_2  與  x  無相關。

## **(e) 最小平方法：條件最小平方**

略去下標  i 的情況下，平方和函數為：

$$
S(\pi_1, \pi_2 \mid \mathbf{y}, \mathbf{x}) = \sum (y_2 - \pi_1 x_1 - \pi_2 x_2)^2
$$

對參數求一階導數如下：

對 π1 偏微：

$$
\frac{\partial S(\pi_1, \pi_2 \mid \mathbf{y}, \mathbf{x})}{\partial \pi_1}
= 2 \sum (y_2 - \pi_1 x_1 - \pi_2 x_2) x_1 = 0
$$

對 π2 偏微：

$$
\frac{\partial S(\pi_1, \pi_2 \mid \mathbf{y}, \mathbf{x})}{\partial \pi_2}
= 2 \sum (y_2 - \pi_1 x_1 - \pi_2 x_2) x_2 = 0
$$

這些為 **最小平方法 (OLS)** 條件，用以求解  π^1,  π^2 的常見正規方程式（normal equations）。

## **(f) 插入數值求解**

我們根據 OLS 一階條件式得到以下 moment conditions：

$$
\frac{1}{N} \sum x_{i1} (y_2 - \pi_1 x_{i1} - \pi_2 x_{i2}) = 0
$$

$$
\frac{1}{N} \sum x_{i2} (y_2 - \pi_1 x_{i1} - \pi_2 x_{i2}) = 0
$$

展開後得到：

$$
\sum x_{i1} y_{i2} - \pi_1 \sum x_{i1}^2 - \pi_2 \sum x_{i1} x_{i2} = 0
$$

$$
\sum x_{i2} y_{i2} - \pi_1 \sum x_{i1} x_{i2} - \pi_2 \sum x_{i2}^2 = 0
$$

插入已知值計算：

$$
3 - \hat{\pi}_1 * 1 - \pi_2 * 0 = 0 \quad \Rightarrow \quad \hat{\pi}_1 = 3
$$

$$
4 - \hat{\pi}_1 * 0 - \hat{\pi}_2 * 1 = 0 \quad \Rightarrow \quad \hat{\pi}_2 = 4
$$

## **(g) 求 α_1  的 IV 估計量**

已知結構方程式為：

$$
y_1 = \alpha_1 y_2 + e_1
$$

且：

$$
\hat{y}_2 = \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2
$$

根據 moment condition（即 \( E[\hat{y}_2 (y_1 - \alpha_1 y_2)] = 0 \)），可得：

$$
\sum \hat{y}_2 (y_1 - \alpha_1 y_2) = 0 
\Rightarrow 
\sum \hat{y}_2 y_1 - \alpha_1 \sum \hat{y}_2 y_2 = 0 
\Rightarrow 
\alpha_1 = \frac{\sum \hat{y}_2 y_1}{\sum \hat{y}_2 y_2}
$$

代入 

$$
\( \hat{y}_2 = \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2 \)
$$

，得：

<img width="484" alt="g" src="https://github.com/user-attachments/assets/8960a10d-6288-4de8-a801-6902cb45e77f" />


## **(h) 證明 Two-Stage Least Squares 解與 IV 解相同**

已知：

$$
\hat{\alpha}_{1, \text{2SLS}} = \frac{\sum \hat{y}_2 y_1}{\sum \hat{y}_2^2}
$$

要證明 

$$
\( \hat{\alpha}_{1, \text{2SLS}} = \hat{\alpha}_1 \)
$$

根據 moment condition，我們需證明：

$$
\sum \hat{y}_2^2 = \sum \hat{y}_2 y_2
$$

由以下推導：

$$
\sum \hat{y}_2^2 = \sum \hat{y}_2 (y_2 - \hat{v}_2)
= \sum \hat{y}_2 y_2 - \sum \hat{y}_2 \hat{v}_2
= \sum \hat{y}_2 y_2
$$

因為：

$$
\sum \hat{y}_2 \hat{v}_2 = 0
$$

這是因為在 OLS 中，預測值 y^_2  與誤差項 v^_2  正交，  
亦即「解釋變數與誤差無關」的基本性質。

------
# 11.16
![image](https://github.com/user-attachments/assets/f320cc53-436d-42b4-b2a7-4f6ebef224a1)
我們考慮以下的結構模型：

<img width="398" alt="1" src="https://github.com/user-attachments/assets/b32594f1-0500-4ff4-be52-b3e370447d3d" />


其中：
- \( Q \)：數量（quantity）
- \( P \)：價格（price, 內生變數）
- \( W \)：工資率（wage rate, 外生變數）
- \( e_d, e_s \)：誤差項（結構誤差）


## **(a) 推導簡約形式（Reduced-Form Equations）**

**Demand = Supply**

$$
\alpha_1 + \alpha_2 P_i + \varepsilon_{di} = \beta_1 + \beta_2 P_i + \beta_3 W_i + \varepsilon_{si}
$$

$$
(\alpha_2 - \beta_2) P_i = (\beta_1 - \alpha_1) + \beta_3 W_i + (\varepsilon_{si} - \varepsilon_{di})
$$

$$
P_i = \frac{\beta_1 - \alpha_1}{\alpha_2 - \beta_2} + \frac{\beta_3}{\alpha_2 - \beta_2} W_i + \frac{\varepsilon_{si} - \varepsilon_{di}}{\alpha_2 - \beta_2}
$$

$$
Q_i = \alpha_1 + \alpha_2 \left( \frac{\beta_1 - \alpha_1}{\alpha_2 - \beta_2} + \frac{\beta_3}{\alpha_2 - \beta_2} W_i + \frac{\varepsilon_{si} - \varepsilon_{di}}{\alpha_2 - \beta_2} \right) + \varepsilon_{di}
$$

$$
Q_i = \alpha_1 + \frac{\alpha_2 (\beta_1 - \alpha_1)}{\alpha_2 - \beta_2} + \frac{\alpha_2 \beta_3}{\alpha_2 - \beta_2} W_i + \frac{\alpha_2 (\varepsilon_{si} - \varepsilon_{di})}{\alpha_2 - \beta_2} + \varepsilon_{di}
$$

## **(b) 哪一個結構方程是可識別的（Identified）？**

M = 2，省略至少一個變數的情況

若 M = 2，且省略了至少一個變數：

- **方程式 (i)** ⇒ 有一個工具變數 ⇒ **可識別（identified）** ⇒ 可以推論出 α_1, α_2
- **方程式 (ii)** ⇒ 無工具變數（zero）⇒ **不可識別（not identified）** ⇒ 無法推論出 β_1, β_2, β_3

## **(c) 使用間接最小平方法（Indirect Least Squares, ILS）**


已知：

$$
\hat{Q} = 5 + 0.5W,\quad \hat{P} = 2 + 4 + 1W
$$

將 $\hat{P}$ 代入 $Q = \alpha_1 + \alpha_2 P$：

$$
5 + 0.5W = \alpha_1 + \alpha_2 (2 + 4 + W)
$$

化簡得：

$$
5 + 0.5W = (\alpha_1 + \alpha_2 \times 6) + \alpha_2 W
$$

比較係數可得：

$$
\alpha_2 = 0.5,\quad 5 = \alpha_1 + 0.5 \times 6
$$

解得：

$$
\alpha_1 = 2
$$

## **(d) 使用 2SLS（二階段最小平方法）估計需求方程**

已知估計式：

$$
\hat{P} = 2 + 4 + W = 6 + W
$$

| $W$ | $\hat{P}$ | $\hat{P} - \bar{\hat{P}}$ | $Q - \bar{Q}$ |
|-----|-----------|---------------------------|---------------|
| 2   | 4.9       | 0                         | -2            |
| 3   | 5.4       | 1                         | 0             |
| 1   | 3.4       | -1                        | 3             |
| 1   | 3.4       | -1                        | 3             |
| 3   | 5.4       | 1                         | 2             |

平均數：

$$
\bar{\hat{P}} = 4.4,\quad \bar{Q} = 6
$$

迴歸模型：

$$
Q = \alpha_1 + \alpha_2 \hat{P} + \varepsilon_i
$$

估計斜率 $\hat{\alpha}_2$ 為：

$$
\hat{\alpha}_2 = \frac{\sum (\hat{P}_i - \bar{\hat{P}})(Q_i - \bar{Q})}{\sum (\hat{P}_i - \bar{\hat{P}})^2}
= \frac{(-1)(3) + (-1)(3) + (1)(0) + (1)(2)}{(-1)^2 + (-1)^2 + (1)^2 + (1)^2}
= \frac{-3 + 3 + 2}{4} = \frac{2}{4} = 0.5
$$

估計截距 $\hat{\alpha}_1$ 為：

$$
\hat{\alpha}_1 = \bar{Q} - \hat{\alpha}_2 \bar{\hat{P}} = 6 - 0.5 \times 4.4 = 3.8
$$

因此，估計的迴歸方程為：

$$
\hat{Q} = 3.8 + 0.5\hat{P}
$$

------
# 11.17
![image](https://github.com/user-attachments/assets/b2dbd597-4653-4be6-aa76-5bc0bb56e965)
本解題基於 Klein's Model I，一個經典的三方程小型總體經濟模型，分析其識別性（identification）與 2SLS（二階段最小平方法）估計方式。


📐 模型結構：三條結構方程式

1️⃣ **消費函數（Consumption Function）**

$$
CN_t = \alpha_1 + \alpha_2 (W_{1t} + W_{2t}) + \alpha_3 P_t + \alpha_4 P_{t-1} + e_{1t}
$$

2️⃣ **投資函數（Investment Function）**

$$
I_t = \beta_1 + \beta_2 P_t + \beta_3 P_{t-1} + \beta_4 K_{t-1} + e_{2t}
$$

3️⃣ **私部門薪資函數（Private Sector Wage Function）**

$$
W_{1t} = \gamma_1 + \gamma_2 E_t + \gamma_3 E_{t-1} + \gamma_4 TIME_t + e_{3t}
$$


## **(a) 工具變數是否足夠？是否滿足識別條件？**

已知 M = 8，需省略 7 個變數，總共 16 個變數。

- 消費函數（Consumption function）包含 6 個變數，並省略了 10 個變數
- 投資函數（Investment function）省略了 11 個變數
- 工資函數（Wage function）省略了 11 個變數

所有方程皆滿足識別（identification）的必要條件。

**(b) 等價的識別條件：排除外生變數的數量檢查**

- **消費函數（Consumption function）**：包含 2 個內生變數（endogenous variables），且排除了 5 個外生變數（exogenous variables）
- **投資函數（Investment function）**：排除 5 個右邊變數（RHS）
- **工資函數（Wage function）**：排除 5 個右邊變數（RHS）

所有識別條件皆滿足（all satisfied）。

## **(c) 私部門薪資  W1t  的一階段簡約形式（First-Stage）**

工資函數（Wage function）為：

$$
W_{it} = \pi_1 + \pi_2 G_{it} + \pi_3 W2_{it} + \pi_4 TX_{t} + \pi_5 TIME_{t} + \pi_6 B_{it} + \pi_7 K_{it} + \pi_8 E_{it} + \nu
$$

其中：

- W_it ：第 i 個單位在時間 t 的工資水準  
- G_it, W2_it, TX_t, TIME_t, B_it, K_it, E_it ：為解釋變數  
- nu ：誤差項

## **(d) 2SLS 估計消費函數的兩個步驟**

從 (c) 題中取得預測值 W^_1，並用與 P^ 相同的方法，建立：

W_t^* = W^_1t + W_2t


接著以 OLS 方法對 CNE 進行回歸。

## **(e) 手動 2SLS 與軟體套件結果是否相同？**

迴歸係數（coefficients）會相同，但 t 值（t-values）則不會相同。

