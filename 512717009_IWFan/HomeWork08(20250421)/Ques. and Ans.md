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

**(a)** 係數符號的預期

| 變數 | 預期符號 | 經濟直覺 |
|------|----------|----------|
| **WAGE** | **+** | 工資提高 → 休閒時間機會成本上升（替代效果）→ 勞動供給時數增加 |
| **EDUC** | **+** | 教育越高 → 技能與職涯機會提升 → 工作誘因與報酬增加 |
| **AGE**  | **±** | 年輕至中年：職涯累積 → 正向；較高年齡：趨近退休、偏好休閒 → 負向（可能呈倒 U 形） |
| **KIDSL6** | **−** | 6 歲以下孩童需求照顧 → 家務時間增加 → 市場工作時數減少 |
| **NWIFEINC** | **−** | 家庭已有其他收入 → 純收入效果使勞動供給意願下降 |

---

**(b)** 為何 OLS 估計不一致

**內生性**  

 時薪 WAGE 受供給與需求雙方決定，且與誤差項 𝑒 中的未觀察能力、偏好等因素相關 → Cov(𝑊𝐴𝐺𝐸,𝑒)≠0。

 OLS 假設 𝐸(𝑒∣𝑋)=0 被破壞 → 估計量偏誤且不一致。
   

**同時性** (Simultaneity)： 勞動市場工資與供給時數同時由需求、供給決定，OLS 無法分離因果方向。  

---

**(c)** 為何 **EXPER、EXPER²** 可作為 **WAGE** 的工具變數

1. **相關性（Relevance）**  

    勞動市場經驗直接影響工資，平方項捕捉報酬遞減 → Cov(EXPER,WAGE) ≠ 0
  
3. **外生性（Exogeneity）**  
   -在控制 AGE、KIDSL6、NWIFEINC、EDUC 等變數後，婦女的「經驗年數」多由過去累積，理論上不直接影響當期休閒/家務偏好，相對不易與當期誤差 𝑒 相關。

    若經驗並未受當期未觀察偏好影響，則 Cov(𝐸𝑋𝑃𝐸𝑅,𝑒)=0  → 滿足 IV 邏輯，可用以識別 WAGE 的因果效果。

---

**(d)** 方程式是否識別

- 內生變數：1 個（WAGE）  
- 工具變數：2 個（EXPER、EXPER²）  
- **階數條件**：工具數 ≥ 內生數 → 2 ≥ 1（滿足）  
- **秩條件**：標準檢定為 det[𝐸(𝑍𝑋′)]≠ 0  ,  EXPER 與 WAGE 一階段關聯顯著即成立  
→ 模型 **過度識別**，可進行 IV/2SLS 估計與過度識別檢定。

---

**(e)** 取得 IV／2SLS 估計的步驟

1. **第一階段 (First Stage)**
   
  <img width="540" alt="2" src="https://github.com/user-attachments/assets/88b404cf-ece5-4df1-a367-50826ea40ab0" />


2. **檢查工具相關性 (Instrument Relevance)**  
   
   檢視第一階段 F‑statistic（>10 通常視為強工具）。

3. **第二階段 (Second Stage)**  

<img width="569" alt="1" src="https://github.com/user-attachments/assets/c716e806-806e-4aea-92ba-443f1d629f58" />

   
4. **估計正確的標準誤**  

   使用 2SLS / IV **robust** 樣本外推公式（例如 White 或 Newey–West）計算標準誤與信賴區間。

5. **診斷檢定 (Diagnostics)**  

   * **過度識別檢定**：(Sargan/Hansen J‑test)
     
   $$
   \begin{aligned}
   H_0 &: \text{工具變數外生} vs. 
   H_1 &: \text{至少一個工具變數不外生}
   \end{aligned}
   $$

   * **弱工具檢定**：比較第一階段 F-statistic 與 Stock–Yogo 臨界值。  
   * **內生性檢定**：Durbin–Wu–Hausman test，比較 OLS 與 2SLS 估計差異。


-----

## Q.03

<img width="672" alt="Q03" src="https://github.com/user-attachments/assets/9e1dd20f-9ef1-45d0-bbe0-718d0e59992d" />

## 題目 10.3（工具變數與間接最小平方法）

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
將 $\beta_2 = \frac{\text{Cov}(z, y)}{\text{Cov}(z, x)}$ 的分母除以 $\text{Var}(z)$，並說明：

$$
\frac{\text{Cov}(z, x)}{\text{Var}(z)}
$$

是將 $x$ 作為因變數、$z$ 為解釋變數之簡單線性回歸：

$$
x = \gamma_1 + \theta_1 z + \nu
$$

的迴歸係數。

> 🔎 提示：請參閱第 10.2.1 節。這即是兩階段最小平方法（2SLS）中的「第一階段」迴歸。


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

由最小平方法估計：

$$
\hat{\theta}_1 = \frac{\text{Cov}(z, x)}{\text{Var}(z)}
$$

因此， $\frac{\text{Cov}(z, x)}{\text{Var}(z)} = \theta_1$

這正是兩階段最小平方法（2SLS）中的第一階段回歸。



**(b)** 證明 $\frac{\text{Cov}(z, y)}{\text{Var}(z)}$ 是另一個回歸係數

考慮簡單線性回歸：

$$
y = \pi_0 + \pi_1 z + u
$$

由 OLS 估計：

$$
\hat{\pi}_1 = \frac{\text{Cov}(z, y)}{\text{Var}(z)}
$$

因此， $\frac{\text{Cov}(z, y)}{\text{Var}(z)} = \pi_1$

這是 reduced-form 回歸：$y$ 對 $z$ 的回歸。



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



**(d)** 證明 $\beta_2 = \frac{\pi_1}{\theta_1}$

由上一步可得：

$$
\pi_1 = \beta_2 \theta_1
\quad \Rightarrow \quad
\beta_2 = \frac{\pi_1}{\theta_1}
$$



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




