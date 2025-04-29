## Q.02

<img width="685" alt="Q02" src="https://github.com/user-attachments/assets/2f8c95dd-c817-4930-bf55-78ae2f2e7045" />

已婚婦女的勞動供給一直是經濟研究的重要議題。請考慮以下的勞動供給方程式：

HOURS=𝛽1+𝛽2WAGE+𝛽3EDUC+𝛽4AGE+𝛽5KIDSL6+𝛽6NWIFEINC+𝑒

其中 HOURS 為勞動供給時數，WAGE 為時薪，EDUC 為受教育年數，KIDSL6 為家中 6 歲以下孩童數量，NWIFEINC 為妻子以外來源的家庭收入。

a. 說明你對各係數符號的預期。

b. 解釋為何此供給方程式無法用 OLS 取得一致估計值。

c. 若以婦女的勞動市場經驗 EXPER 及其平方 EXPER² 作為 WAGE 的工具變數，說明它們符合工具變數邏輯的原因。

d. 此供給方程式是否識別？請說明。

e. 敘述取得 IV/2SLS 估計值的步驟（非電腦指令）。

## Ans:

考慮以下已婚女性的勞動供給迴歸模型：

$$
HOURS = \beta_1 + \beta_2 WAGE + \beta_3 EDUC + \beta_4 AGE + \beta_5 KIDSL6 + \beta_6 NWIFEINC + e
$$

其中：
- **HOURS**：每週工作小時數（勞動供給）
- **WAGE**：時薪（每小時工資）
- **EDUC**：教育年數
- **AGE**：年齡
- **KIDSL6**：家庭中 6 歲以下兒童數
- **NWIFEINC**：除女性勞動所得外的家庭收入
- **e**：誤差項


**(a)** 各變數的預期符號與解釋

| 變數 | 預期符號 | 解釋 |
|------|----------|------|
| β2  (WAGE) |   +   | 較高工資提高工作誘因（替代效果） |
| β3  (EDUC) |   +   | 教育程度高者具備更佳工作機會與收入潛力 |
| β4  (AGE)  |   ±   | 年齡可能正向影響經驗，但年長者亦可能減少工時 |
| β5  (KIDSL6) |   -   | 幼兒照護責任增加，降低勞動供給 |
| β6  (NWIFEINC) |   -   | 家庭其他收入增加，女性工作動機減弱（收入效果） |

---

**(b)** 為什麼 OLS 無法一致估計該模型？

原因在於變數 **WAGE** 是 **內生變數**，可能與誤差項 \( e \) 有關聯，例如：

- 能力強 → 工資高 → 同時也傾向從事更多工作（勞動供給 ↑）
- 工時和工資彼此影響（反向因果）

導致違反 OLS 基本假設：

$$
\text{Cov}(WAGE, e) \ne 0
$$

➡️ **OLS 估計量有偏且不一致。**

---

**(c)** 為何 EXPER 與 EXPER² 可作為 WAGE 的工具變數？

工具變數需符合兩大條件：

| 判準 | 是否符合？ | 解釋 |
|------|------------|------|
| **關聯性（Relevance）** | ✅ 是 | 工作年資通常顯著影響工資 |
| **排除性（Exogeneity）** | ✅ 可接受 | 經驗不應直接影響工時，僅間接透過工資發揮影響力 |

➡️ EXPER 與 EXPER² 是合理的工具變數選擇。

---

**(d)** 模型是否具識別性？

條件如下：

- 內生變數數量：1 個（WAGE）
- 工具變數數量：2 個（EXPER, EXPER²）

定義：
當工具變數數量 ≥ 內生變數數量 → 模型為「過度識別」或「正好識別」

在這裡：

1 個內生變數

2 個工具變數

➡️ 模型為 **過度識別（overidentified）**

➡️ 模型 **具識別性**，可以進一步檢驗工具變數有效性（例如 Sargan 檢定）

---

**(e)** 取得 IV/2SLS 估計的步驟（非程式語法）

第一步：第一階段回歸（first stage）

將內生變數 WAGE 迴歸於工具變數：

$$
WAGE = \gamma_0 + \gamma_1 \cdot EXPER + \gamma_2 \cdot EXPER^2 + \text{controls} + v
$$

計算出預測值 $\widehat{WAGE}$


第二步：第二階段回歸（second stage）

將 $\widehat{WAGE}$ 代入原始模型：

$$
HOURS = \beta_1 + \beta_2 \cdot \widehat{WAGE} + \beta_3 \cdot EDUC + \beta_4 \cdot AGE + \beta_5 \cdot KIDSL6 + \beta_6 \cdot NWIFEINC + u
$$

透過 OLS 估計上式，所得到的 $\hat{\beta}_2$ 即為 **工具變數法（二階段最小平方法）** 的估計值。


-----

## Q.03

<img width="672" alt="Q03" src="https://github.com/user-attachments/assets/9e1dd20f-9ef1-45d0-bbe0-718d0e59992d" />

**工具變數與間接最小平方法**

在迴歸模型：

$$
y = \beta_1 + \beta_2 x + e
$$

中，假設 $x$ 是內生變數，且 $z$ 是一個有效的工具變數。

在第 10.3.5 節中，我們已知：

$$
\beta_2 = \frac{\text{Cov}(z, y)}{\text{Cov}(z, x)}
$$

請完成以下各小題：

**(a)**

將下列公式中 $\beta_2 = \frac{\text{Cov}(z, y)}{\text{Cov}(z, x)}$ 的**分母**除以 $\text{Var}(z)$，並證明：

$$
\frac{\text{Cov}(z, x)}{\text{Var}(z)}
$$

是將變數 $x$ 對 $z$ 進行**簡單線性迴歸**時所得到的**迴歸係數**，其迴歸模型為：

$$
x = \gamma_1 + \theta_1 z + \nu
$$

> 🔎 *提示：請參考第 10.2.1 節，這是二階段最小平方法（2SLS）中的第一階段迴歸。*


**(b)**
將 $\beta_2 = \frac{\text{Cov}(z, y)}{\text{Cov}(z, x)}$ 的分子除以 $\text{Var}(z)$，並說明：

$$
\frac{\text{Cov}(z, y)}{\text{Var}(z)}
$$

是將 $y$ 作為因變數、$z$ 為解釋變數之簡單線性回歸：

$$
y = \pi_0 + \pi_1 z + u
$$

的迴歸係數。

> 🔎 提示：請參閱第 10.2.1 節。


**(c)**
在原始模型：

$$
y = \beta_1 + \beta_2 x + e
$$

中，使用 $x = \gamma_1 + \theta_1 z + \nu$ 代入，推導出：

$$
y = \pi_0 + \pi_1 z + u
$$

的形式。請表示 $\pi_0$、$\pi_1$、$u$ 分別為原始參數與誤差項的函數。

此結果為 **簡約形式（reduced-form）** 的方程式。


**(d)**
請證明：

$$
\beta_2 = \frac{\pi_1}{\theta_1}
$$


**(e)**
若 $\hat{\pi}_1$ 與 $\hat{\theta}_1$ 分別為 $\pi_1$ 與 $\theta_1$ 的 OLS 估計值，請證明：

$$
\hat{\beta}_2 = \frac{\hat{\pi}_1}{\hat{\theta}_1}
$$

為 $\beta_2$ 的一致估計量（consistent estimator）。

這個估計量稱為 **間接最小平方法（indirect least squares, ILS）估計量**。

----

## Ans:

在回歸模型：

$$
y = \beta_1 + \beta_2 x + e
$$

中，假設 $x$ 是內生變數，$z$ 是一個有效的工具變數。

我們知道：

$$
\beta_2 = \frac{\text{Cov}(z, y)}{\text{Cov}(z, x)}
$$

**(a)** 證明 $\frac{\text{Cov}(z, x)}{\text{Var}(z)}$ 是一個回歸係數

考慮簡單線性回歸：

$$
x = \gamma_1 + \theta_1 z + \nu
$$


對兩邊取期望：

   $$
   E(x) = \gamma_1 + \theta_1 E(z)
   $$

兩邊減去期望：

   $$
   x - E(x) = \theta_1 (z - E(z)) + \nu
   $$

將等式兩邊都乘上 $z - E(z)$：

   $$
   (z - E(z))(x - E(x)) = \theta_1 (z - E(z))^2 + \nu (z - E(z))
   $$

對整個式子取期望值：

   $$
   E\left[(z - E(z))(x - E(x))\right] = \theta_1 E\left[(z - E(z))^2\right] + E\left[\nu (z - E(z))\right]
   $$

假設 $\nu$ 與 $z$ 無關（工具變數的基本假設），則右邊的誤差項期望為 0：

   $$
   E[\nu(z - E(z))] = 0
   $$

   所以得：

   $$
   E\left[(z - E(z))(x - E(x))\right] = \theta_1 E\left[(z - E(z))^2\right]
   $$

解出 $\theta_1$：**

   $$
   \theta_1 = \frac{E[(z - E(z))(x - E(x))]}{E[(z - E(z))^2]} = \frac{\text{Cov}(z, x)}{\text{Var}(z)}
   $$

------

**(b)** 證明 $\frac{\text{Cov}(z, y)}{\text{Var}(z)}$ 是另一個回歸係數

考慮簡單線性回歸：

$$
y = \pi_0 + \pi_1 z + u
$$


對兩邊取期望值

$$
E(y) = \pi_0 + \pi_1 E(z)
$$


兩邊減去期望

$$
y - E(y) = \pi_1 (z - E(z)) + u
$$


兩邊乘上 $z - E(z)$

$$
(z - E(z))(y - E(y)) = \pi_1 (z - E(z))^2 + u (z - E(z))
$$


對整個式子取期望值：

$$
E[(z - E(z))(y - E(y))] = \pi_1 E[(z - E(z))^2] + E[u (z - E(z))]
$$


假設 $u$ 與 $z$ 無關（IV 基本假設）

$$
E[u (z - E(z))] = 0
$$

因此有：

$$
E[(z - E(z))(y - E(y))] = \pi_1 E[(z - E(z))^2]
$$


解出 $\pi_1$：

$$
\pi_1 = \frac{E[(z - E(z))(y - E(y))]}{E[(z - E(z))^2]} = \frac{\text{Cov}(z, y)}{\text{Var}(z)}
$$

------

**(c)** 推導 reduced-form 方程式

從原始模型：

$$
y = \beta_1 + \beta_2 x + e
$$

將第一階段回歸 $x = \gamma_1 + \theta_1 z + \nu$ 代入得：

$$
y = \beta_1 + \beta_2 (\gamma_1 + \theta_1 z + \nu) + e \\
= \beta_1 + \beta_2 \gamma_1 + \beta_2 \theta_1 z + \beta_2 \nu + e
$$

令：

- $\pi_0 = \beta_1 + \beta_2 \gamma_1$
- $\pi_1 = \beta_2 \theta_1$
- $u = \beta_2 \nu + e$

得到：

$$
y = \pi_0 + \pi_1 z + u
$$

這是 reduced-form 模型。

-----

**(d)** 證明 $\beta_2 = \frac{\pi_1}{\theta_1}$

由上一步可得：

$$
\pi_1 = \beta_2 \theta_1
\quad \Rightarrow \quad
\beta_2 = \frac{\pi_1}{\theta_1}
$$

-----

**(e)** 證明間接最小平方法估計量為一致估計量

若 $\hat{\pi}_1$ 與 $\hat{\theta}_1$ 是 OLS 的一致估計量，則：

$$
\hat{\beta}_2 = \frac{\hat{\pi}_1}{\hat{\theta}_1}
$$

由於：

- $\hat{\pi}_1 \xrightarrow{p} \pi_1$
- $\hat{\theta}_1 \xrightarrow{p} \theta_1$

根據連續映射定理（Continuous Mapping Theorem）：

$$
\hat{\beta}_2 = \frac{\hat{\pi}_1}{\hat{\theta}_1} \xrightarrow{p} \frac{\pi_1}{\theta_1} = \beta_2
$$

因此， $\hat{\beta}_2$ 是 $\beta_2$ 的一致估計量，這稱為 **indirect least squares (ILS)** 估計量。

---




