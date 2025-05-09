## C11Q01

<img width="652" alt="C11Q01" src="https://github.com/user-attachments/assets/a2248d85-3166-47bd-a741-0686105e6171" />

---

## 中文翻譯

我們的目標是估計下列聯立方程模型中的參數：

$$
y_1 = \alpha_1 y_2 + e_1 \\
$$

$$
y_2 = \alpha_2 y_1 + \beta_1 x_1 + \beta_2 x_2 + e_2
$$

我們假設 \( x_1 \)、\( x_2 \) 是外生變數，與誤差項 \( e_1 \)、\( e_2 \) 無相關。

（a）求解 \( y_2 \) 的簡約式方程：

$$
y_2 = \pi_1 x_1 + \pi_2 x_2 + \nu_2
$$

請將簡約式係數表示為結構參數的函數，並將誤差項表示為 \( e_1 \)、\( e_2 \) 的函數。請證明 \( y_2 \) 與 \( e_1 \) 有相關性。

（b）哪個方程中的參數可以用 OLS 一致估計？請說明理由。

（c）根據聯立方程中的識別性定義，哪些參數是可識別的？請說明理由。

（d）欲使用「矩估法（MOM）」估計 \( y_2 \) 的簡約式參數，根據第 10.3 節，可得兩個矩條件如下：

$$
\frac{1}{N} \sum x_{1i}(y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0 
$$

$$
\frac{1}{N} \sum x_{2i}(y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0
$$

請解釋這兩個條件為何可以一致估計簡約式參數。

（e）第 (d) 小題中的 MOM 估計量是否與 OLS 相同？請對 

$$
\( y_2 = \pi_1 x_1 + \pi_2 x_2 + \nu_2 \) 
$$

進行最小平方法推導，證明導數設為零後等同於 MOM 矩條件。

（f）
給定：

$$
\( \sum x_{1i}^2 = 1 \)
\( \sum x_{2i}^2 = 1 \)
\( \sum x_{1i} x_{2i} = 0 \)
\( \sum x_{1i} y_i = 2 \)
\( \sum x_{1i} y_{2i} = 3 \)
\( \sum x_{2i} y_i = 3 \)
\( \sum x_{2i} y_{2i} = 4 \)
$$

請利用 (d) 的矩條件求得 

$$
\( \hat{\pi}_1 = 3 \)、\( \hat{\pi}_2 = 4 \)
$$

（g）
令：

$$
\hat{y}_2 = \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2
$$

請說明為何以下矩條件可以一致估計 α₁ ：

Σx₂ᵢ(ŷ₁ᵢ − α₁y₂ᵢ) = 0

並求出 IV 估計量 α₁。

（h）
使用下列式子進行 OLS，找出 α_1 的 2SLS 估計值：

$$
y_1 = \alpha_1 \hat{y}_2 + e_1^*
$$

並與 (g) 小題結果比較。

---------

## ANS:

**(a) 消去與代入法：推導 reduced form**

方程組：

$$
y_1 = \alpha_1 y_2 + e_1 \\
$$

$$
y_2 = \alpha_2 y_1 + \beta_1 x_1 + \beta_2 x_2 + e_2
$$

將方程式 (1) 代入方程式 (2) 並化簡：

$$
\[
\begin{aligned}
y_2 &= \alpha_2 y_1 + \beta_1 x_1 + \beta_2 x_2 + e_2 \\
&= \alpha_2 (\alpha_1 y_2 + e_1) + \beta_1 x_1 + \beta_2 x_2 + e_2 \\
&= \alpha_1 \alpha_2 y_2 + \alpha_2 e_1 + \beta_1 x_1 + \beta_2 x_2 + e_2
\end{aligned}
\]
$$

將 \( y_2 \) 移至左邊：

$$
\[
y_2 (1 - \alpha_1 \alpha_2) = \beta_1 x_1 + \beta_2 x_2 + e_2 + \alpha_2 e_1
\]
$$

Solve for \( y_2 \):

$$
\[
y_2 = \frac{\beta_1}{1 - \alpha_1 \alpha_2} x_1 + \frac{\beta_2}{1 - \alpha_1 \alpha_2} x_2 + \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2}
\]
$$

定義：

$$
\[
\pi_1 = \frac{\beta_1}{1 - \alpha_1 \alpha_2}, \quad 
\pi_2 = \frac{\beta_2}{1 - \alpha_1 \alpha_2}, \quad 
v_2 = \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2}
\]
$$

則：

$$
\[
y_2 = \pi_1 x_1 + \pi_2 x_2 + v_2
\]
$$

 To show the correlation:

$$
\[
\begin{aligned}
\text{cov}(y_2, e_1 \mid \mathbf{x}) 
&= E(y_2 e_1 \mid \mathbf{x}) \\
&= E \left[ \left( \frac{\beta_1}{1 - \alpha_1 \alpha_2} x_1 + \frac{\beta_2}{1 - \alpha_1 \alpha_2} x_2 + \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2} \right) e_1 \,\middle|\, \mathbf{x} \right] \\
&= E \left[ \frac{\beta_1}{1 - \alpha_1 \alpha_2} x_1 e_1 \mid \mathbf{x} \right] + E \left[ \frac{\beta_2}{1 - \alpha_1 \alpha_2} x_2 e_1 \mid \mathbf{x} \right] + E \left[ \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2} e_1 \mid \mathbf{x} \right] \\
&= 0 + 0 + E \left[ \frac{e_2 e_1 + \alpha_2 e_1^2}{1 - \alpha_1 \alpha_2} \mid \mathbf{x} \right] \\
&= \frac{\alpha_2 \sigma_{e_1}^2}{1 - \alpha_1 \alpha_2} \quad \text{(若 } e_1 \text{ 與 } e_2 \text{ 不相關)}
\end{aligned}
\]
$$

因此，當 \( \alpha_2 \ne 0 \) 且 \( \alpha_1 \ne 0 \) 時，\( y_2 \) 與 \( e_1 \) 相關 ⇒ 說明內生性。

我們已得：

$$
\[
\text{cov}(y_2, e_1 \mid \mathbf{x}) = E(y_2 e_1 \mid \mathbf{x})
\]
$$

接著，

$$
\[
\begin{aligned}
\text{cov}(y_2, e_1 \mid \mathbf{x}) 
&= \frac{E(e_2 e_1 \mid \mathbf{x}) + \alpha_2 E(e_1^2 \mid \mathbf{x})}{1 - \alpha_1 \alpha_2} \\
&= \frac{\alpha_2}{1 - \alpha_1 \alpha_2} \sigma_1^2
\end{aligned}
\]
$$

此共變數非零，除非：

$$
\[
\alpha_2 = 0 \Rightarrow \text{無同時性問題 (no simultaneity)}
$$



**(b) OLS 可一致估計的條件**

因為原始兩個結構式的右邊都包含內生變數（如 \( y_1 \)），所以不能直接使用 OLS。

但 reduced form 中只有外生變數 \( x_1, x_2 \)，可以對其進行 OLS 一致估計。


**(c) 識別性（Identification）**

結構模型中 \( M = 2 \) 個方程式。為了識別一條方程式，必須省略 \( M - 1 = 1 \) 個外生變數。

- 方程 (1) 中已省略 \( x_1, x_2 \)，是 **已識別**
- 方程 (2) 中未省略任何外生變數，**未被識別**


**(d) Moment Conditions**

這些 moment conditions 來自於 \( x \) 為外生變數（exogenous）的假設。因此有：

$$
\[
E(x_{i1} v_{i1} \mid \mathbf{x}) = E(x_{i2} v_{i2} \mid \mathbf{x}) = 0
\]
$$

根據 part (a) 的結果，\( y_2 \) 的 reduced form 為：

$$
\[
y_2 = \frac{\beta_1}{1 - \alpha_1 \alpha_2} x_1 + \frac{\beta_2}{1 - \alpha_1 \alpha_2} x_2 + \frac{e_2 + \alpha_2 e_1}{1 - \alpha_1 \alpha_2}
\]
$$

簡記為：

$$
\[
y_2 = \pi_1 x_1 + \pi_2 x_2 + v_2
\]
$$

Reduced Form 誤差項與 \( x \) 無相關性證明：

我們要證明：

<img width="495" alt="d" src="https://github.com/user-attachments/assets/8fd9f1e9-f1b4-44f2-a14f-8e530680cf4f" />


這是因為 \( x \) 為外生變數，對 \( e_1, e_2 \) 條件期望為 0。

因此，reduced form 的誤差項 \( v_2 \) 與 \( x \) 無相關。


**(e) 最小平方法：條件最小平方**

略去下標 \( i \) 的情況下，平方和函數為：

$$
\[
S(\pi_1, \pi_2 \mid \mathbf{y}, \mathbf{x}) = \sum (y_2 - \pi_1 x_1 - \pi_2 x_2)^2
\]
$$

對參數求一階導數如下：

$$
\[
\frac{\partial S(\pi_1, \pi_2 \mid \mathbf{y}, \mathbf{x})}{\partial \pi_1}
= 2 \sum (y_2 - \pi_1 x_1 - \pi_2 x_2) x_1 = 0
\]
$$

$$
\[
\frac{\partial S(\pi_1, \pi_2 \mid \mathbf{y}, \mathbf{x})}{\partial \pi_2}
= 2 \sum (y_2 - \pi_1 x_1 - \pi_2 x_2) x_2 = 0
\]
$$

這些為 **最小平方法 (OLS)** 條件，用以求解 \( \hat{\pi}_1, \hat{\pi}_2 \) 的常見正規方程式（normal equations）。

---

**(f) 插入數值求解**

我們根據 OLS 一階條件式得到以下 moment conditions：

$$
\[
\frac{1}{N} \sum x_{i1} (y_2 - \pi_1 x_{i1} - \pi_2 x_{i2}) = 0
\]
\[
\frac{1}{N} \sum x_{i2} (y_2 - \pi_1 x_{i1} - \pi_2 x_{i2}) = 0
\]
$$

展開後得到：

$$
\[
\sum x_{i1} y_{i2} - \pi_1 \sum x_{i1}^2 - \pi_2 \sum x_{i1} x_{i2} = 0
\]
\[
\sum x_{i2} y_{i2} - \pi_1 \sum x_{i1} x_{i2} - \pi_2 \sum x_{i2}^2 = 0
\]
$$

插入已知值計算：

$$
\[
3 - \hat{\pi}_1 = 0 \quad \Rightarrow \quad \hat{\pi}_1 = 3
\]
\[
4 - \hat{\pi}_2 = 0 \quad \Rightarrow \quad \hat{\pi}_2 = 4
\]
$$

---

**(g) 求 \( \alpha_1 \) 的 IV 估計量**

第一條結構方程式為：

$$
\[
y_1 = \alpha_1 y_2 + e_1
\]
$$

因此，

$$
\[
E \left[ (\pi_1 x_1 + \pi_2 x_2) e_1 \mid \mathbf{x} \right] 
= E \left[ (\pi_1 x_1 + \pi_2 x_2)(y_1 - \alpha_1 y_2) \mid \mathbf{x} \right] = 0
\]
$$

這個 moment condition 的經驗型形式為：

$$
\[
\frac{1}{N} \sum (\pi_1 x_{i1} + \pi_2 x_{i2})(y_{i1} - \alpha_1 y_{i2}) = 0
\]
$$

如果我們知道 \( \pi_1 \) 與 \( \pi_2 \)，就可以解出 \( \alpha_1 \) 的估計量。

雖然我們不知道真實參數，但可以從 reduced form 中一致估計：

$$
\[
\text{plim } \hat{\pi}_1 = \pi_1, \quad \text{plim } \hat{\pi}_2 = \pi_2
\]
$$

因此，將估計值代入後，有：

$$
\[
\sum (\hat{\pi}_1 x_{i1} + \hat{\pi}_2 x_{i2})(y_{i1} - \alpha_1 y_{i2}) = \sum \hat{y}_{i2} (y_{i1} - \alpha_1 y_{i2}) = 0
\]
$$

整理得：

$$
\[
\sum \hat{y}_{i2} y_{i1} - \alpha_1 \sum \hat{y}_{i2} y_{i2} = 0
\quad \Rightarrow \quad
\hat{\alpha}_{1, IV} = \frac{\sum \hat{y}_{i2} y_{i1}}{\sum \hat{y}_{i2} y_{i2}}
\]
$$

**(h) 使用 SLS 的估計結果**

若設：

- \( x = \hat{y}_2 \)，
- \( y = y_1 \)

無截距的最小平方法估計為：

$$
\hat{\alpha}_1 = \frac{\sum y_i x_i}{\sum x_i^2} = \frac{\sum y_{1i} \hat{y}_{2i}}{\sum \hat{y}_{2i}^2}
$$

這與上一題 (g) 得到的 IV 估計一致，根據：

- \( \sum x v = 0 \)：OLS 的正交性質

---





























































































考慮以下同時方程模型：

(1) y1=α1y2+e1

​(2) y2=α2y1+β1x1+β2x2+e2
​
假設 x1, x2  是外生變數，且與誤差項 e1 、 e2  無關。

--

**(a) 推導 \( y_2 \) 的簡約形式（Reduced Form）**

將 (1) 代入 (2) 得：

y2=α2(α1y2+e1)+β1x1+β2x2+e2​

y2−α1α2​y2=α2e1+β1x1+β2x2+e2

(1−α1α2)y2=β1x1+β2x2+α2e1+e2
​
定義：

$$
\( \pi_1 = \frac{\beta_1}{1 - \alpha_1 \alpha_2} \)
\( \pi_2 = \frac{\beta_2}{1 - \alpha_1 \alpha_2} \)
\( v_2 = \frac{\alpha_2 e_1 + e_2}{1 - \alpha_1 \alpha_2} \)
$$

則簡約式為：

$$
y_2 = \pi_1 x_1 + \pi_2 x_2 + v_2
$$

<img width="282" alt="1" src="https://github.com/user-attachments/assets/654c7a79-55d6-4830-95b6-96c137949e2e" />

--

**(b) 哪些參數可由 OLS 一致估計？**

𝑦1=𝛼1𝑦2+𝑒1：不能用 OLS，因為 𝑦2與e1相依。

𝑦2=𝛼2𝑦1+𝛽1𝑥1+𝛽2𝑥2+𝑒2：不能用 OLS，因為 𝑦1 亦受 𝑦2影響。

因此：無法使用 OLS 一致估計任一結構參數。

--

**(c) 模型的「可識別性」判斷**

模型 (1) 僅含 𝑦2，但不含任何 𝑥 ⇒ 無外生變數 ⇒ 未識別

模型 (2) 含 𝑥1,𝑥2 ⇒ 足夠外生變數 ⇒ 可識別

所以：只有方程 (2) 的結構參數是可識別的。

--

**(d) 方法矩估計（MOM）**

根據簡約形式：

$$
y_2 = \pi_1 x_1 + \pi_2 x_2 + v_2
$$

設下兩個矩條件：

$$
\begin{align*}
\frac{1}{N} \sum x_{1i} (y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0 \\\\
\frac{1}{N} \sum x_{2i} (y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0
\end{align*}
$$

𝐸[𝑥1𝑖𝑣2𝑖]=0, 𝐸[𝑥2𝑖𝑣2𝑖]=0  外生變數對誤差項無關聯 ⇒ valid instruments

這些是兩個有效的 moment 條件，可用來一致估計 𝜋1,𝜋2。

--

**(e) MOM 與 OLS 是否相同？**

OLS 最小化：

$$
\sum (y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i})^2
$$

一階導數設為 0 得到：

$$
\sum x_{1i}(y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0 
$$

$$
\sum x_{2i}(y_{2i} - \pi_1 x_{1i} - \pi_2 x_{2i}) = 0
$$

所以：OLS 與 MOM 是一致的估計方式（在這線性條件下）

--

**(f) 計算 π^1, 𝜋^2**

<img width="429" alt="2" src="https://github.com/user-attachments/assets/6fbb3041-ab15-478d-833f-174b88464bec" />


✅ MOM 與 OLS 結果一致

--

**(g) 解釋下列矩條件為何合理：**

$$
\sum x_{2i}(y_{1i} - \alpha_1 \hat{y}_{2i}) = 0
$$

其中：

$$
\hat{y}_2 = \hat{\pi}_1 x_1 + \hat{\pi}_2 x_2 = 2x_1 + 1.5x_2
$$

這對應於用 x1, x2  做為  y2  的工具，來估計：

$$
y_1 = \alpha_1 \hat{y}_2 + \text{誤差}
$$

✅ ⇒ 𝑥2與 𝑒1∗無關 ⇒ 由於 x2  為外生 ⇒ 與誤差無關 ⇒ 矩條件合理有效

--

**(h) 利用 2SLS 估計 𝛼1)**

第二階段：

$$
y_1 = \alpha_1 \hat{y}_2 + \epsilon_1
$$

其中 y^2 = 2x1 + 1.5x2  來自第一階段  
→ 此步即為 2SLS，與 (g) 中矩條件結果相符

✅ 所得的  𝛼^1為一致估計值


------------------

## C11Q16

<img width="680" alt="C11Q16" src="https://github.com/user-attachments/assets/8ec6ccf6-e2ce-4fd0-adfa-916a89273346" />


---

## 中文翻譯

考慮以下供給與需求模型：

<img width="398" alt="1" src="https://github.com/user-attachments/assets/aae3fa43-81cf-45a1-902f-094575421ee6" />

​
- 其中 Q 為數量，P 為價格， W 為工資水準，並假設為外生變數。  

這些變數的資料如表格 11.7 所示。

（a）推導簡約式（Reduced-form）方程的代數形式：  

  Q = θ₁ + θ₂W + ν₂ 以及 P = π₁ + π₂W + ν₁ 

請將簡約式參數表示為結構參數的函數。

（b）根據 (a) 的結果，能求出哪些結構參數？哪一條方程是「可識別的（identified）」？

（c）已知簡約式估計為： 

 Q̂ = 5 + 0.5W 及 P̂ = 2.4 + 1W

請求出可識別的結構參數。這就是 **間接最小平方法（indirect least squares）**。

(d）根據簡約式中 \( P \) 的預測值，應用 **兩階段最小平方法（2SLS）** 來估計需求方程的參數。


---------

## ANS:

我們考慮以下的結構模型：

<img width="398" alt="1" src="https://github.com/user-attachments/assets/b32594f1-0500-4ff4-be52-b3e370447d3d" />


其中：
- \( Q \)：數量（quantity）
- \( P \)：價格（price, 內生變數）
- \( W \)：工資率（wage rate, 外生變數）
- \( e_d, e_s \)：誤差項（結構誤差）


**(a) 推導簡約形式（Reduced-Form Equations）**

我們希望得到：

$$
Q = \theta_1 + \theta_2 W + v_2 
$$

$$
P = \pi_1 + \pi_2 W + v_1
$$

**Ⅰ**

<img width="446" alt="2" src="https://github.com/user-attachments/assets/27678c3d-01f8-49d2-aac8-c4406ee847b0" />


**Ⅱ**

<img width="449" alt="3" src="https://github.com/user-attachments/assets/03b798ed-caba-4ecc-a265-71fb9996429d" />

--

**(b) 哪一個結構方程是可識別的（Identified）？**

使用「識別條件（Order condition）」進行判斷：

- **需求方程**（Demand）：
  - 包含內生變數 \( P \)
  - 排除變數 \( W \)（工具變數存在）✅
  - ⇒ **可識別**

- **供給方程**（Supply）：
  - 包含所有外生變數，無被排除變數 ❌
  - ⇒ **不可識別**

✅ 結論：**僅 demand 方程是識別的**

--

**(c) 使用間接最小平方法（Indirect Least Squares, ILS）**

題目給定的簡約估計式：

$$
\hat{Q} = 5 + 0.5 W \\
\hat{P} = 2.4 + 1 \cdot W
$$

即：

𝜃1=5,𝜃2=0.5 

𝜋1=2.4,𝜋2=1

根據結構推導關係：

$$
\theta_1 = \alpha_1 + \alpha_2 \pi_1 \\
\theta_2 = \alpha_2 \pi_2
$$

代入數值：

5=𝛼1+𝛼2(2.4)

0.5=𝛼2(1)

解得：

𝛼2=0.5

𝛼1=5−0.5×2.4=3.8

✅ 間接最小平方法結果：

𝛼^1=3.8

𝛼^2=0.5

--

**(d) 使用 2SLS（二階段最小平方法）估計需求方程**

第一步：回歸 𝑃∼𝑊

根據簡約形式：

$$
\hat{P} = \pi_1 + \pi_2 W = 2.4 + 1 \cdot W
$$

對每個 \( W = \{2, 3, 1, 1, 3\} \)，得到：

| W | P^ |
|--------|--------------|
| 2      | 4.4          |
| 3      | 5.4          |
| 1      | 3.4          |
| 1      | 3.4          |
| 3      | 5.4          |


第二步：回歸 𝑄∼𝑃^

對每個 \( Q = \{4, 6, 9, 3, 8\} \), 得到：

| Q  | P^ |
|--------|--------------|
| 4      | 4.4          |
| 6      | 5.4          |
| 9      | 3.4          |
| 3      | 3.4          |
| 8      | 5.4          |

估計模型：

$$
Q = \alpha_1 + \alpha_2 \hat{P} + u
$$

這就是使用 2SLS 所得到的 demand 方程結構參數估計。  
其結果應與 (c) 的間接最小平方法估計值相近。


------------

## C11Q17

<img width="680" alt="C11Q17" src="https://github.com/user-attachments/assets/1522b0b8-8489-4e58-9246-aab7372a9d61" />


---


**繁體中文翻譯**

範例 11.3 介紹 Klein 的模型 I。

（a）我們是否有足夠的工具變數（IVs）來估計每一條方程式？  請檢查每一方程的識別必要條件。  在  M  條方程的系統中，每條方程至少要省略  M - 1  個變數，才符合識別的必要條件。

（b）另一個等價的識別條件是：每條方程中被排除的外生變數數量，  必須至少等於包含在右邊的內生變數數量。  請檢查每條方程是否符合此條件。

（c）請寫出工人私人部門薪資 W_{1t} 的第一階段方程式（簡約式）， 以計量經濟學符號表示，參數記為  π₁、π₂⋯ 。

（d）說明消費函數的兩階段最小平方法（2SLS）估計步驟。  這不是關於電腦軟體指令的問題。

（e）請問按照（d）步驟所得到的回歸結果，是否與專門用於 2SLS 估計的軟體所得結果一致？  尤其是  t  值會相同嗎？


---------------

## ANS

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


**(a) 工具變數是否足夠？是否滿足識別條件？**

必要條件：系統中若有 \( M \) 條方程，每一條需排除至少 \( M - 1 \) 個變數。

此處 \( M = 3 \)，所以每條方程需排除至少 **2 個工具變數**。

我們有：

- 8 個內生變數（例如：CN, I, W1, P, E, E-1 等）
- 8 個外生變數（例如： G, W2, TX, TIME, P-1, K-1, E-1, 常數項 ）

✅ **結論**：每個結構方程皆有足夠排除變數 → 全部可識別

--

**(b) 等價的識別條件：排除外生變數的數量檢查**

條件：方程中「排除的外生變數數量」應 ≥ 「右側內生變數數量」。

各方程檢查：

- 消費方程包含： W1, P, P-1) ⇒ 排除至少兩個外生變數 ✅  
- 投資方程包含： P, P-1  ⇒ 排除  W1, CN, TIME, TX  等 ✅  
- 私部門薪資方程包含：Et, Et-1 ⇒ 排除 CN, I, Pt  等 ✅  

✅ **結論**：每個方程都滿足此條件 ⇒ 可識別

--

**(c) 私部門薪資  W1t  的一階段簡約形式（First-Stage）**

以計量符號表示為：

$$
W_{1t} = \pi_1 + \pi_2 G_t + \pi_3 W_{2t} + \pi_4 TX_t + \pi_5 K_{t-1} + \pi_6 P_{t-1} + \pi_7 E_{t-1} + \pi_8 TIME_t + v_{1t}
$$

這是一個典型的「使用所有外生變數」作為工具變數的 reduced-form 方程。

--

**(d) 2SLS 估計消費函數的兩個步驟（非程式面）**

Step 1️⃣：預測內生變數

回歸每個右側的內生變數（如 𝑊1𝑡,𝑃𝑡W ）對所有外生變數（即工具變數），建立 OLS 回歸模型。

程式碼區塊（以 𝑃為例）：

P_hat <- lm(P ~ G + W2 + TX + K_lag + TIME + E_lag, data = ...)$fitted.values

這代表：

用外生變數（G, W2, TX, K_lag, TIME, E_lag）來解釋內生變數 𝑃 透過 lm() 進行 OLS 回歸 

fitted.values 代表的是 𝑃^：這就是第二階段中要使用的工具變數形式

得到 𝑃^𝑡,𝑊^1𝑡

​Step 2️⃣：以預測值進行主方程回歸

將預測值 𝑃^𝑡,𝑊^1𝑡 ​代入消費方程，估計：

𝐶𝑁𝑡=𝛼1+𝛼2(𝑊^1𝑡+𝑊2𝑡)+𝛼3𝑃^𝑡+𝛼4𝑃𝑡−1+𝑢𝑡

透過 OLS 即可得一致估計量。

--

**(e) 手動 2SLS 與軟體套件結果是否相同？**

- ✅ **迴歸係數（估計值）會相同**
- ⚠️ **標準誤與 t 值不一定相同**

原因：

- 軟體（如 `AER::ivreg()`）會使用 **heteroskedasticity-robust** 標準誤（sandwich estimator）
- 若你手動執行 2SLS，需額外計算 robust 標準誤，否則 t 值會不同

✅ **結論**：
- 手動與軟體的係數一致
- t 值僅在使用相同標準誤處理下才會一致

---












