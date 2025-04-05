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

**(b)**

在 5% 顯著水準下，檢定虛無假設：

$$
H_0: \beta_1 + 2\beta_2 = 5 \\
H_1: \beta_1 + 2\beta_2 \ne 5
$$

這是一個 **線性組合參數的檢定（General Linear Hypothesis）**，使用 $t$ 統計量進行推論。

Step 1：建立線性組合

設：

$$
g = \beta_1 + 2\beta_2 \Rightarrow \hat{g} = b_1 + 2b_2
$$


Step 2：代入估計值

由題目得：

$$
b_1 = 2, \quad b_2 = 3 \\
\Rightarrow \hat{g} = 2 + 2 \cdot 3 = 8
$$


計算 $\text{Var}(\hat{g})$（使用 Delta Method）

$$
\text{Var}(\hat{g}) = \text{Var}(b_1 + 2b_2) = \text{Var}(b_1) + 4 \cdot \text{Var}(b_2) + 4 \cdot \text{Cov}(b_1, b_2)
$$

查共變異數矩陣得：

$$
\text{Var}(b_1) = 3, \quad \text{Var}(b_2) = 4, \quad \text{Cov}(b_1, b_2) = -2
$$

代入計算：

$$
\text{Var}(\hat{g}) = 3 + 4 \cdot 4 + 4 \cdot (-2) = 3 + 16 - 8 = 11
$$

因此標準誤為：

$$
\text{se}(\hat{g}) = \sqrt{11} \approx 3.317
$$

計算 t 統計量

$$
t = \frac{\hat{g} - 5}{\text{se}(\hat{g})} = \frac{8 - 5}{3.317} \approx 0.905
$$

查表與做決策

- 自由度：$df = 63 - 3 = 60$
- 臨界值：$t_{0.025, 60} \approx 2.000$


結論

$$
|t| = 0.905 < 2.000 \Rightarrow \text{無法拒絕 } H_0
$$

因此，在 5% 顯著水準下，**沒有足夠的證據拒絕** $\beta_1 + 2\beta_2 = 5$，結果落在接受域中。



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




**(a)** 估計迴歸模型與係數解釋

$$
\[
\hat{\text{TIME}} = 20.8701 + 0.36813 \cdot \text{DEPART} + 1.5219 \cdot \text{REDS} + 3.0237 \cdot \text{TRAINS}
\]
$$

**解釋：**

- 常數項（20.8701）：當Bill 早上 6:30 離開卡內基，且沒有遇到紅燈和火車時，他的預期通勤時間估計為 20.8701 分鐘。
- DEPART（0.36813）：Bill每晚於早上 6:30  1分鐘，他的預期旅行時間就會增加 0.3681 分鐘（假設紅燈和火車數量保持不變）。
- REDS（1.5219）：在出發時間和火車數量保持不變的情况下，每個紅燈預計增加的旅行時間估計為 1.5219 分鐘。
- TRAINS（3.0237）：在出發時間和紅燈數量保持不变的情況下，每列火車預計增加的旅行時間估計為 3.0237 分鐘。

---

**(b)**  


<img src="https://github.com/user-attachments/assets/342447a0-258a-4b89-8e24-49d9a2fc80a1" alt="圖片描述" width="250" height="150" />



The 95% confidence intervals for the coefficients are: 

 β1 :   b1 ± t(0.975,245)se(b1)=20.8701 ± 1.970 × 1.6758 = (17.57,24.17) 
 
 β2 :   b2 ± t(0.975,245)se(b2)=0.36813 ± 1.970 × 0.03510 = (0.299,0.437)
 
 β3 :   b3 ± t(0.975,245)se(b3)=1.5219 ± 1.970 × 0.1850 = (1.16,1.89)
 
 β4 :   b4 ± t(0.975,245)se(b4)=3.0237 ± 1.970 × 0.6340 = (1.77,4.27)

---

**(c)** 紅燈延遲是否小於 2 分鐘？(左尾檢定)


- \( H0: β3 ≧ 2 )，\( H1: β3 < 2 )
- \( t {0.05,245} = -1.651  ,  t = (1.5219-2)/ 0.1850 = -2.584

   p-value = 0.004889295 

結論： -2.584 ＜ -1.651，拒絕H0，每個紅燈的預期延遲時間少於2分鐘。


<img src="https://github.com/user-attachments/assets/4325d09a-a1ea-4867-8204-2db9b38f0aa6" alt="圖片描述" width="500" height="350" />


---



**(d)**  火車延遲是否不等於 3 分鐘？(雙尾檢定，顯著水準 10%)


- \( H0:  β4 = 3 )，\( H1:  β4 ≠ 3 )
- \( t {0.05,245} = -1.651  ,  t {0.95,245} = 1.651
- t = (3.0237-3)/ 0.6340 = 0.037

  p-value = 0.9701865 

結論： -1.651＜0.037＜1.651，無法拒絕H0，每列火車的預期延誤時間為 3 分鐘的假設一致。



<img src="https://github.com/user-attachments/assets/1dab1c78-83e9-4650-9d45-ca595b2035ed" alt="圖片描述" width="500" height="350" />


---

**(e)**  7:30 出發是否會多花至少 10 分鐘？(左尾檢定)


- \( H0: 30β2 ≧ 10  vs.  H2: 30β2 ＜ 10  → β2 ≧ 0.3333 )
- \( t = {0.36813 - 0.3333}/{0.03510} = 0.991 )

p-value = 0.8394291 

結論： 0.991 > -1.651 , 無法拒絕H0，延遲出發時間 30 分鐘會導致預期旅行時間至少增加 10 分鐘。



<img src="https://github.com/user-attachments/assets/6e3547d1-7b6d-4bd0-8e4a-aa61af4115da" alt="圖片描述" width="500" height="350" />



---

**(f)**  火車延遲是否小於紅燈延遲的 3 倍？


- \( H0: β4 ≧ 3β3 and  H1: β4 ＜ 3β3   )
- t = (b4-3b3)/ se(b4-3b3)= (3.0237-3×1.5219)/0.8450 = -1.825

se(3b3-b4)= √ 9×var(b3)+var(b4)-2×3×cov(b2,b3)

          =√ 9×0.0342391+0.401971-6×cov0.0006482

          =0.8450

結論：-1.825＜-1.651  , 拒絕H0，火車的預期延誤小於紅燈延誤的三倍。



<img src="https://github.com/user-attachments/assets/d42627c4-8094-43d6-bf33-e194e58f37ee" alt="圖片描述" width="500" height="350" />


---

**(g)**  Bill 7:00 出發，遇 6 紅燈 1 火車，能否在 7:45 前到？(右尾檢定)

- \( H0: β1+30β2+6β3+β4 ≦ 45  VS.  H1: β1+30β2+6β3+β4 ﹥ 45 )

- t = {0.95,245}= 1.651 ,

    = （b1+30b2+6b3+b4-45）/se(b1+30b2+6b3+b4)

    = -0.93076 / 0.53927

    =-1.726

   p-value = 0.957823
  
結論：-1.726＜-1.651  , 無法拒絕H0 ，因為 p-value = Pr(t (245))＞ -1.726) = 0.9572，大於 0.05。沒有足夠的證據證明 Bill 會在 7:45 AM 之後到達大學。



<img src="https://github.com/user-attachments/assets/59d59aae-33d0-4d3d-a655-5f6a02a84719" alt="圖片描述" width="500" height="350" />



---

**(h)**  若 Bill 必須確保不遲到 7:45 AM

H0: β1+30β2+6β3+β4 ＞ 45 (存在遲到風險) VS.  H1: β1+30β2+6β3+β4 ＜ 45 (確保準時)。

t = {0.05,245}= -1.651 

結論：在這種情況下，我们拒绝 H0,因為-1.726 < −1.651 , Bill 預計通勤時間足以讓他准時参加會議。



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



<img src="https://github.com/user-attachments/assets/a0ed50b2-407e-4d94-b8c7-75b59919a83c" alt="圖片描述" width="600" height="320" />







**(a)**係數是否顯著不同於零？

所有係數估計值在 1% 的顯著性小平下均顯不同於零，除了 2 EDUC 的系數估計值在 12% 顯著。

---

**(b)**教育的邊際效果

$$
\[
\frac{\partial \ln(WAGE)}{\partial EDUC} = \ β2 + 2\ β3 EDUC + \ β6 EXPER
\]
$$

ME EDUC =0.089539+0.002916EDUC-0.001010 EXPER 

教育的邊際效應隨著教育水平的提高而增大，但随着經驗水平的提高而减小。

---

**(c)** 教育邊際效果分布分析
 

大多數人在教育上的邊際報酬介於 2.8%～5% 間，

邊際效應範圍從 0.036 到 0.148，其中大部分集中在 0.085 到 0.13 之間。

 N = 1200
 
 Mean       0.107348
 
 Median     0.108431
 
 Maximum    0.147871
 
 Minimum    0.035654
 
 Std. Dev.   0.017222
 
 Skewness    -0.406617
 
 Kurtosis    3.150502
 
 Jarque-Bera 34.20008
 
 Probability 0.000000


ME(EDUC,0.05) =0.080 , ME(EDUC,0.50) =0.108 , and  ME(EDUC,0.95) =0.134 


這些值是通過邊際效應進行排序選擇第 60、第 600 和第 1140 個觀測值獲得的。


<img src="https://github.com/user-attachments/assets/b8d28165-cea4-4037-85b4-b3d7ff69ae88" alt="圖片描述" width="600" height="320" />


---

**(d)** 經驗的邊際效果（對 log(WAGE)）

$$
\[
\frac{\partial \ln(WAGE)}{\partial EXPER} = \ β4 + 2\ β5 EXPER + \ β6 EDUC
\]
$$


ME EXPER = 0.044879 - 0.000936 EXPER - 0.001010 EDUC 


經驗的邊際效應隨著教育水平的提高和工作年的增加而减小。



---

**(e)** 經驗邊際效果分布分析

 n = 1200
 Mean     0.008652
 
 Median   0.008419
 
 Maximum  0.033989
 
 Minimum -0.025279
 
 Std. Dev.   0.012170
 
 Skewness  -0.046597
 
 Kurtosis   2.123526
 
 Jarque-Bera 38.84459
 
 Probability 0.000000

結論：雖然經驗的邊際效應大部分是正的，但也有一部分（28.3%）是負的，範圍從 -0.025  到 0.034。

ME(EXPER,0.05) = -0.010 , ME(EXPER,0.50) = 0.0008 , and  ME(EXPER,0.95) =0.028 



<img src="https://github.com/user-attachments/assets/c4a478b0-877d-47ab-9eb1-8dba63cc20d7" alt="圖片描述" width="600" height="320" />


---

**(f)** 比較 David (17y 教育、8y 經驗) 與 Svetlana (16y 教育、18y 經驗)

Svetlana 的期望對數工資：

$$
E[\ln(WAGE) \mid EDUC = 16, EXPER = 18] =
\beta_1 + 16\beta_2 + 16^2\beta_3 + 18\beta_4 + 18^2\beta_5 + (16 \times 18)\beta_6
$$

David 的期望對數工資：

$$
E[\ln(WAGE) \mid EDUC = 17, EXPER = 8] =
\beta_1 + 17\beta_2 + 17^2\beta_3 + 8\beta_4 + 8^2\beta_5 + (17 \times 8)\beta_6
$$

比較二者之差異：

Svetlana 的期望對數工資大於或等於 David 的條件為：

$$
(16 - 17)\beta_2 + (16^2 - 17^2)\beta_3 + (18 - 8)\beta_4 + (18^2 - 8^2)\beta_5 + [(16 \cdot 18) - (17 \cdot 8)]\beta_6 \geq 0
$$


檢定虛無與對立假設

將上式簡化為線性組合：

$$
H_0: -\beta_2 - 33\beta_3 + 10\beta_4 + 260\beta_5 + 152\beta_6 \geq 0
$$

$$
H_1: -\beta_2 - 33\beta_3 + 10\beta_4 + 260\beta_5 + 152\beta_6 < 0
$$

（注意：這樣設定的 \( H_1 \) 代表 Svetlana 的對數工資較低，即 David 較高）


統計量與檢定結果

計算的 t 統計量為：

$$
t = \frac{-b_2 - 33b_3 + 10b_4 + 260b_5 + 152b_6}
{se(-b_2 - 33b_3 + 10b_4 + 260b_5 + 152b_6)}
$$

實際帶入後：

- 計算值：\\( t = \{0.035885}{0.021489} = 1.670 \\)
- 臨界值（單尾 5%，自由度約 1194）：\\( t_{0.05,1194} = -1.646 \\)

因為：

$$
1.670 > -1.646
$$

結論：

我們**無法拒絕虛無假設 \( H_0 \)**，代表沒有足夠證據說明 David 的期望對數工資顯著高於 Svetlana。


---

**(g)** 過了 8 年後再比較（教育不變，經驗 +8）

新的比較情境

- **David**：教育 17 年，原本經驗 8 年，增加 8 年後變成 **16 年**
- **Svetlana**：教育 16 年，原本經驗 18 年，增加 8 年後變成 **26 年**

期望對數工資差異（Svetlana > David）之條件：

$$
(16 - 17)\beta_2 + (16^2 - 17^2)\beta_3 + (26 - 16)\beta_4 + (26^2 - 16^2)\beta_5 + [(16 \cdot 26) - (17 \cdot 16)]\beta_6 \geq 0
$$

整理為線性假設形式：

$$
H_0: -\beta_2 - 33\beta_3 + 10\beta_4 + 420\beta_5 + 144\beta_6 \geq 0
$$

$$
H_1: -\beta_2 - 33\beta_3 + 10\beta_4 + 420\beta_5 + 144\beta_6 < 0
$$


檢定統計量與計算

t 統計量為：

$$
t = \frac{-b_2 - 33b_3 + 10b_4 + 420b_5 + 144b_6}
{se(-b_2 - 33b_3 + 10b_4 + 420b_5 + 144b_6)} \leq t_{(0.05, 1194)} = -1.646
$$

已知計算結果為：

- t 值：\\( -0.030917 / 0.014991 = -2.062 \\)
- 判斷：\\( -2.062 < -1.646 \\)


結論

我們**拒絕虛無假設 \( H_0 \)**，表示 David 的期望對數工資**顯著高於** Svetlana。

這與第 (f) 小題的結論不同，原因為：

> **報酬遞減**：Svetlana 原先已有 18 年經驗，額外 8 年的邊際效益較小；而 David 原僅 8 年經驗，新增 8 年對他的工資影響更大。


---

**(h)** 檢定 Wendy vs. Jill 的經驗邊際效果是否相等

Wendy：

$$
\frac{\partial E[\ln(WAGE)]}{\partial EXPER} \bigg|_{EDUC=12,\, EXPER=17}
= \beta_4 + 2 \cdot 17 \beta_5 + 12 \beta_6
$$

Jill：

$$
\frac{\partial E[\ln(WAGE)]}{\partial EXPER} \bigg|_{EDUC=16,\, EXPER=11}
= \beta_4 + 2 \cdot 11 \beta_5 + 16 \beta_6
$$

使兩者邊際效果相等之條件

令二者邊際效果相等：

$$
2(17 - 11)\beta_5 + (12 - 16)\beta_6 = 0
\Rightarrow 12\beta_5 - 4\beta_6 = 0
$$

檢定虛無與對立假設

$$
H_0: 12\beta_5 - 4\beta_6 = 0
\quad vs. \quad
H_1: 12\beta_5 - 4\beta_6 \ne 0
$$

拒絕區間：若

$$
\left| t \right| = \left| \frac{12b_5 - 4b_6}{se(12b_5 - 4b_6)} \right| \geq 1.962
$$

則拒絕 \( H_0 \)

實際檢定結果

- 計算值：
- 
$$
t = \frac{-0.001575}{0.001533} = -1.027
$$

- 臨界值（雙尾 5%）：±1.962

因為：

$$
-1.962 < -1.027 < 1.962
$$

結論：

我們**無法拒絕虛無假設 \( H_0 \)**  
→ **沒有證據顯示 Wendy 與 Jill 的經驗邊際效果不同**

---

**(i)** Jill 的經驗邊際效果何時變為負？


邊際效果為：

$$
ME_{EXPER} = \frac{\partial E[\ln(WAGE)]}{\partial EXPER}
= \beta_4 + 2\beta_5 \cdot EXPER + \beta_6 \cdot EDUC
$$

對於 Jill（EDUC = 16）來說，當邊際效果為 0 時的條件為：

$$
\beta_4 + 2\beta_5 \cdot EXPER + 16\beta_6 = 0
$$

解得：

$$
EXPER^* = \frac{-\beta_4 + 16\beta_6}{2\beta_5}
$$


點估計與標準誤

代入估計值得到：

$$
\hat{EXPER}^* = \frac{-b_4 + 16b_6}{2b_5} = 30.677
$$

Jill 現在已有 11 年經驗，距離邊際效果轉為負值的年數為：

$$
30.677 - 11 = 19.677 \text{ 年}
$$


標準誤與 Delta Method

用 Delta method 計算其標準誤為：

$$
se(\hat{EXPER}^*) = 1.8957
$$


95% 信賴區間估計

使用公式：

$$
\hat{EXPER}^* - 11 \pm 1.962 \cdot se = 19.677 \pm 1.962 \cdot 1.8957
= [15.96, 23.40]
$$


結論

Jill 還需要大約 **19.68 年** 的經驗，她的邊際效果才會轉為負值。  
在 95% 信心水準下，該年數區間為 **[15.96 年, 23.40 年]**。




<img src="https://github.com/user-attachments/assets/c27dd0ff-b357-4dd1-a03a-38a78effc38c" alt="圖片描述" width="500" height="350" />







