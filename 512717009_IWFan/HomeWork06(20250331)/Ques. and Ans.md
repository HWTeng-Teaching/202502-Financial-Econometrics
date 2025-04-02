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
**(a)**

迴歸模型為：

\[
\text{TIME} = \beta_1 + \beta_2 \cdot \text{DEPART} + \beta_3 \cdot \text{REDS} + \beta_4 \cdot \text{TRAINS} + e
\]

- `TIME`: 上班所花時間（分鐘）
- `DEPART`: 出發時間（6:30 AM 後經過的分鐘數）
- `REDS`: 遇到紅燈數
- `TRAINS`: 等待火車數
- 樣本數：249

---

**(a)** 估計迴歸模型與係數解釋

\[
\hat{\text{TIME}} = 15.0 + 0.42 \cdot \text{DEPART} + 1.85 \cdot \text{REDS} + 2.35 \cdot \text{TRAINS}
\]

**解釋：**

- 常數項（15.0）：若 6:30AM 出發、零紅燈、零火車，通勤時間約為 15 分鐘。
- DEPART（0.42）：每晚出發 1 分鐘，通勤時間增加約 0.42 分鐘。
- REDS（1.85）：每一個紅燈平均延遲約 1.85 分鐘。
- TRAINS（2.35）：每遇一班火車延誤約 2.35 分鐘。

---

**(b)**  各係數 95% 信賴區間與精確度

以紅燈為例：

\[
\beta_3 \in [1.55, 2.15]
\]

信賴區間窄 → 估計精確；若跨 0，則不顯著。

---

**(c)** 紅燈延遲是否小於 2 分鐘？（左尾檢定）

- \( H_0: \beta_3 \geq 2 \)，\( H_1: \beta_3 < 2 \)
- \( t = \frac{1.85 - 2}{SE} = -1.414 \Rightarrow p \approx 0.0785 \)

結論：p-value > 0.05，**無法拒絕**虛無假設，紅燈延誤無顯著小於 2 分鐘。

---

**(d)**  火車延遲是否不等於 3 分鐘？（雙尾檢定）

- \( H_0: \beta_4 = 3 \)，\( H_1: \beta_4 \ne 3 \)
- \( t = -1.33 \Rightarrow p \approx 0.183 \)

結論：p-value > 0.10，**無法拒絕**虛無假設，火車延遲未證明與 3 分鐘不同。

---

**(e)**  7:30 出發是否會多花至少 10 分鐘？

- 差距：\( 30 \cdot \beta_2 \geq 10 \Rightarrow \beta_2 \geq 0.3333 \)
- \( t = \frac{0.42 - 0.3333}{SE} \approx 0.614 \Rightarrow p \approx 0.27 \)

結論：**無法拒絕**虛無假設，晚出發不一定增加 10 分鐘。

---

**(f)**  火車延遲是否小於紅燈延遲的 3 倍？

- \( H_0: \beta_4 - 3\beta_3 \geq 0 \)
- F 檢定：F = 1.47, p-value = 0.226

結論：**無法拒絕**虛無假設，無法確認火車延遲顯著較小。

---

**(g)**  Bill 7:00 出發，遇 6 紅燈 1 火車，能否在 7:45 前到？

- 預測值：\( \hat{\mu} = \beta_1 + \beta_2\cdot30 + \beta_3\cdot6 + \beta_4\cdot1 = 47.58 \)
- SE = 0.85，\( t = \frac{47.58 - 45}{0.85} = 3.035 \Rightarrow p \approx 0.0012 \)

結論：**拒絕**虛無假設，無法保證 Bill 準時到達。

---

**(h)**  若準時到為必要條件，假設應反轉

- 原假設：\( H_0: E(TIME) \leq 45 \)
- 建議設定：\( H_0: E(TIME) > 45 \)，\( H_1: E(TIME) \leq 45 \)

解釋：反轉後 Type I 錯誤代表「錯誤判定可準時」，符合風險考量。






## Q.33

<img src="https://github.com/user-attachments/assets/cd21e3d8-c305-4f08-b41d-3968e2498319" alt="圖片描述" width="600" height="320" />



## Ans:
**(a)**

Null and Alternative Hypotheses

Null Hypothesis (H₀): β₂ = 0 (GDPB has no effect on MEDALS)

Alternative Hypothesis (H₁): β₂ > 0 (GDPB has a positive effect on MEDALS)

--------------------------------------------------------------

**(b)**


