## Q.06

<img src="https://github.com/user-attachments/assets/7ce8ceae-e277-4aed-9209-8ddd3a09b0b0" alt="圖片描述" width="600" height="320" />

## Ans:

Hypothesis Testing with Estimated Coefficient Covariance Matrix

From a sample of 63 observations, the least squares estimates and the estimated covariance matrix are:

estimated：

```math
\begin{bmatrix}
b_1 \\
b_2 \\
b_3
\end{bmatrix}
=
\begin{bmatrix}
2 \\
3 \\
-1
\end{bmatrix}
```

covariance  matrix：

$$
\widehat{\text{Cov}}(b1, b2, b3) =
\begin{bmatrix}
3 & -2 & 1 \\
-2 & 4 & 0 \\
1 & 0 & 3
\end{bmatrix}
$$

樣本數為 63，我們接下來對三個虛無假設進行檢定，使用 5% 顯著水準。



<img src="https://github.com/user-attachments/assets/bab10ef1-1830-4ba7-a870-ded971ec8896" alt="圖片描述" width="500" height="320" />



--------------------------------------------------------------



<img src="https://github.com/user-attachments/assets/9456a6fc-ddd8-4161-8e36-4ee29587cf5a" alt="圖片描述" width="600" height="600" />




--------------------------------------------------------------




<img src="https://github.com/user-attachments/assets/bbdffcb8-a4b7-460b-972b-3741754f8800" alt="圖片描述" width="900" height="700" />






## Summary Table

| Hypothesis                                | Test Statistic | Critical Value | Conclusion               |
|-------------------------------------------|----------------|----------------|--------------------------|
| \( β2 = 0 \)                              | 1.5            | 2.000          | Fail to reject \( H0 \)  |
| \( β1 + 2β = 5 \)                         | ≈ 0.9045       | 2.000          | Fail to reject \( H0 \)  |
| \( β1 - β2 + β3 = 4 )                     | -1.5           | 2.000          | Fail to reject \( H0 \)  |

> **Note**: All tests are two-tailed with 5% significance level and 60 degrees of freedom.



--------------------------------------------------------------


## Q.31

<img src="https://github.com/user-attachments/assets/80ed1460-637b-49d0-b011-073641d83a50" alt="圖片描述" width="600" height="320" />



## Ans:


<img src="https://github.com/user-attachments/assets/04c6f07c-bba7-4573-b5fb-234d8425596e" alt="圖片描述" width="600" height="320" />




迴歸模型為：

$$
\[
\text{TIME} = \beta_1 + \beta_2 \cdot \text{DEPART} + \beta_3 \cdot \text{REDS} + \beta_4 \cdot \text{TRAINS} + e
\]
$$

- `TIME`: 上班所花時間（分鐘）
- `DEPART`: 出發時間（6:30 AM 後經過的分鐘數）
- `REDS`: 遇到紅燈數
- `TRAINS`: 等待火車數
- 樣本數：249

---

**(a)** 估計迴歸模型與係數解釋

$$
\[
\hat{\text{TIME}} = 20.8701 + 0.36813 \cdot \text{DEPART} + 1.5219 \cdot \text{REDS} + 3.0237 \cdot \text{TRAINS}
\]
$$

**解釋：**

- 常數項（20.8701）：當Bill 早上 6:30 離開卡內基，且沒有遇到紅燈和火車時，他的預期通勤時間估計為 20.87 分鐘。
- DEPART（0.36813）：如果 Bill 晚於早上 6:30 出發，則他的出發時間每晚於早上 6:30  10 分鐘，他的預期旅行時間就會增加 3.7 分鐘（假設紅燈和火車數量保持不變）。
- REDS（1.5219）：在出發時間和火車數量保持不變的情况下，每個紅燈預計增加的旅行時間估計為 1.52 分鐘。
- TRAINS（3.0237）：在出發時間和紅燈數量保持不变的情況下，每列火車預計增加的旅行時間估計為 3.02 分鐘。

---

**(b)**  各係數 95% 信賴區間與精確度

以紅燈為例：

$$
\[
\beta_3 \in [1.55, 2.15]
\]
$$

信賴區間窄 → 估計精確；若跨 0，則不顯著。

---

**(c)** 紅燈延遲是否小於 2 分鐘？（左尾檢定）


- \( H0: β3 ≧ 2 )，\( H1: β3 < 2 )
- \( t = {1.85 - 2}/{SE} = -1.414 \  p-value = 0.0785 )


結論：P = 0.0785 > 0.05，**無法拒絕H0，紅燈延誤無顯著小於 2 分鐘。

---

**(d)**  火車延遲是否不等於 3 分鐘？（雙尾檢定）


- \( H0:  β4 = 3 )，\( H1:  β4 ≠ 3 )
- \( t = -1.33 \ p-value = 0.183 )


結論：P =0.183 > 0.10，**無法拒絕H0，火車延遲未證明與 3 分鐘不同。

---

**(e)**  7:30 出發是否會多花至少 10 分鐘？


- 差距：\( H0: 30β2 ≧ 10  vs.  H2: 30β2 ＜ 10  → β2 ≧ 0.3333 )
- \( t = {0.42 - 0.3333}/{SE} = 0.614 \ p-value = 0.27 \)


結論： P > 0.05**無法拒絕H0，晚出發不一定增加 10 分鐘。

---

**(f)**  火車延遲是否小於紅燈延遲的 3 倍？


- \( H0: β4 ≧ 3β3 → β4 -β3  ≧ 0 )
- F 檢定：F = 1.47, p-value = 0.226


結論：P＞0.05**無法拒絕H0，無法確認火車延遲顯著較小。

---

**(g)**  Bill 7:00 出發，遇 6 紅燈 1 火車，能否在 7:45 前到？

- \( H0: E(TIME)  ≦ 45  VS.  H1: E(TIME)  ﹥ 45 )

- SE = 0.85，\( t = \{47.58 - 45}/ {0.85} = 3.035 \ p-value = 0.0012 \)

結論：P＜0.05 **拒絕H0，代表無法保證準時抵達。

---

**(h)**  若準時到為必要條件，假設應反轉

- 原假設：\( H0: E(TIME) \leq 45 \)
- 建議設定：\( H0: E(TIME) > 45 \（可能遲到）)，\( H_1: E(TIME) ≦ 45 \（確保不會遲到）)

解釋：反轉後 Type I 錯誤代表「錯誤判定可準時」，符合風險考量。



--------------------------------------------------------------


## Q.33

<img src="https://github.com/user-attachments/assets/cd21e3d8-c305-4f08-b41d-3968e2498319" alt="圖片描述" width="600" height="320" />



## Ans:

模型形式為：

$$
\[
\ln(WAGE) = \beta_1 + \beta_2 EDUC + \beta_3 EDUC^2 + \beta_4 EXPER + \beta_5 EXPER^2 + \beta_6 (EDUC \times EXPER) + e
\]
$$

---

**(a)**係數是否顯著不同於零？

使用回歸輸出中的 p-value 判斷，每個係數在 1%、5%、10% 顯著水準下是否顯著。範例輸出如下：

- \( β2 \): p-value = 0.000 → 顯著
- \( β3 \): p-value = 0.000 → 顯著
- \( β4 \): p-value = 0.000 → 顯著
- \( β5 \): p-value = 0.002 → 顯著
- \( β6 \): p-value = 0.009 → 顯著

---

**(b)**教育的邊際效果（對 log(WAGE)）

$$
\[
\frac{\partial \ln(WAGE)}{\partial EDUC} = \ β2 + 2\ β3 EDUC + \ β6 EXPER
\]
$$

教育和經驗同時影響教育的邊際效果。當教育增加時，\( 2\ β3 EDUC \) 變大；當經驗增加，\( \ β6 EXPER \) 變大。

---

**(c)** 教育邊際效果分布分析

計算每位樣本的教育邊際效果，並畫出直方圖。  
統計結果：
- **第 5 百分位**：0.0275
- **中位數**：0.0376
- **第 95 百分位**：0.0492

大多數人在教育上的邊際報酬介於 2.8%～5% 間。



<img src="https://github.com/user-attachments/assets/b8d28165-cea4-4037-85b4-b3d7ff69ae88" alt="圖片描述" width="600" height="320" />


---

**(d)** 經驗的邊際效果（對 log(WAGE)）

$$
\[
\frac{\partial \ln(WAGE)}{\partial EXPER} = \ β4 + 2\ β5 EXPER + \ β6 EDUC
\]
$$

邊際效果受經驗平方與教育交乘項影響。



---

**(e)** 經驗邊際效果分布分析

- **第 5 百分位**：0.0167
- **中位數**：0.0289
- **第 95 百分位**：0.0434

結論：對大多數人而言，增加一年經驗可提升 1.7%～4.3% 工資。



<img src="https://github.com/user-attachments/assets/c4a478b0-877d-47ab-9eb1-8dba63cc20d7" alt="圖片描述" width="600" height="320" />


---

**(f)** 比較 David (17y 教育、8y 經驗) 與 Svetlana (16y 教育、18y 經驗)

- 設 \( \Delta = \mu_D - \mu_S \)
- 利用估計係數計算差異，檢定：

$$
\[
H_0: \Delta \leq 0 \quad vs. \quad H_1: \Delta > 0
\]
$$

結果：
- 差異 \( \hat{\Delta} = 0.130 \)
- t = 2.42, p-value = 0.0089

結論：拒絕虛無假設，David 的預期對數工資顯著高於 Svetlana。

---

**(g)** 過了 8 年後再比較（教育不變，經驗 +8）

David (16y)、Svetlana (26y) 經驗變多後，再計算：

- 新差異 \( \hat{\Delta} = 0.064 \)
- t = 1.31, p-value = 0.096

結論：不再顯著拒絕虛無假設，David 與 Svetlana 的工資預期差異不再顯著。

---

**(h)** 檢定 Wendy vs. Jill 的經驗邊際效果是否相等

- Wendy: educ = 12, exper = 17  
- Jill: educ = 16, exper = 11

差異公式：

$$
\[
ME_W - ME_J = 12\ β5 - 4\ β6 = 4(3\ β5 - \ β6)
\]
$$

檢定：

$$
\[
H_0: 3\ β5 - \ β6 = 0 \quad vs. \quad H_1: \ne 0
\]
$$

F 檢定結果：p-value < 0.05 ⇒ 拒絕虛無假設，兩人經驗邊際效果顯著不同。

---

**(i)** Jill 的經驗邊際效果何時變為負？

邊際效果公式：

$$
\[
ME = \ β4 + 2\ β5 EXPER + \ β6 \cdot 16 = 0
\Rightarrow EXPER^* = -\frac{\beta_4 + 16\beta_6}{2\beta_5}
\]
$$

估計結果：

- \( EXPER^* \approx 31.29 \)
- Jill 現在 11 年經驗，還要約 **20.29 年** 經驗
- 95% 信賴區間為：[18.31, 22.38] 年



