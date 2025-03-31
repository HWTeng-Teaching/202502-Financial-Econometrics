<img src="https://github.com/user-attachments/assets/f65e1f7c-6cf4-4416-8d35-137b7d22901d" alt="圖片描述" width="600" height="150" />

---

<img src="https://github.com/user-attachments/assets/9fa1e23f-d561-4935-a28c-cc05db667e99" alt="圖片描述" width="600" height="450" />

---

<img src="https://github.com/user-attachments/assets/a9aae3aa-4190-4119-ae12-f21533def2db" alt="圖片描述" width="600" height="550" />

---
假設從一個包含 63 筆觀測值的樣本中，取得最小平方法估計值及其對應的估計共變異數矩陣如下：

\[
\begin{pmatrix} b_1 \\ b_2 \\ b_3 \end{pmatrix} = \begin{pmatrix} 2 \\ 3 \\ -1 \end{pmatrix}
\]

共變異數矩陣：

\[
\text{cov}(b_1, b_2, b_3) =
\begin{pmatrix}
3 & -2 & 1 \\
-2 & 4 & 0 \\
1 & 0 & 3
\end{pmatrix}
\]

在 5% 顯著性水準下，假設對立假設為「不等於」，請針對下列虛無假設逐一檢定：

a. \( \beta_2 = 0 \)

b. \( \beta_1 + 2\beta_2 = 5 \)

c. \( \beta_1 - \beta_2 + \beta_3 = 4 \)

---



---
使用資料檔案 `cps5_small` 的觀測值，估計以下模型：

$$
\ln(WAGE) = \beta_1 + \beta_2 \, EDUC + \beta_3 \, EDUC^2 + \beta_4 \, EXPER + \beta_5 \, EXPER^2 + \beta_6 \, (EDUC \times EXPER) + e
$$

**a.** 在哪些顯著水準下，各迴歸係數估計值與零「顯著不同」？

**b.** 推導邊際效果 
$$\frac{\partial E[\ln(WAGE)|EDUC, EXPER]}{\partial EDUC}$$ 
的表達式。說明當 `EDUC` 與 `EXPER` 增加時，該邊際效果的變化趨勢。

**c.** 對樣本中所有觀測值計算 (b) 部分的邊際效果，並繪製其直方圖。你發現了什麼？請找出該分佈的中位數、第 5 百分位數與第 95 百分位數。

**d.** 推導邊際效果 
$$\frac{\partial E[\ln(WAGE)|EDUC, EXPER]}{\partial EXPER}$$ 
的表達式。說明當 `EDUC` 與 `EXPER` 增加時，該邊際效果的變化趨勢。

**e.** 對樣本中所有觀測值計算 (d) 部分的邊際效果，並繪製其直方圖。你發現了什麼？請找出該分佈的中位數、第 5 百分位數與第 95 百分位數。

**f.** David 受教育 17 年、工作經驗 8 年；Svetlana 受教育 16 年、工作經驗 18 年。請在 5% 顯著性水準下，檢定虛無假設：Svetlana 的期望對數薪資大於或等於 David 的對數薪資；對立假設為 David 的對數薪資較高。請以模型參數表達虛無與對立假設。

**g.** 經過 8 年後，David 與 Svetlana 均增加 8 年工作經驗，但教育年數不變。此時 (f) 題的檢定結果是否相同？請解釋其原因。

**h.** Wendy 受教育 12 年、工作經驗 17 年；Jill 受教育 16 年、工作經驗 11 年。請在 5% 顯著性水準下，檢定兩人增加經驗的邊際效果是否相同。請以模型參數表達虛無與對立假設。

**i.** Jill 的邊際經驗效果何時會變為負值？請求出該數值的 95% 區間估計。






