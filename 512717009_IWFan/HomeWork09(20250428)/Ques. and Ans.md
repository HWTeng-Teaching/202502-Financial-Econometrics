### C10Q18

<img width="672" alt="Q18" src="https://github.com/user-attachments/assets/d3655a37-c5ac-446e-9d43-3c1342e5d019" />


10.18 請考慮 working wives 資料檔 mroz。使用 428 位參與勞動市場的已婚婦女的觀測值。本練習探討父母大學教育作為工具變數的有效性。

a. 建立兩個新變數。若母親教育年數 MOTHEREDUC > 12，則 MOTHERCOLL = 1，否則為 0；同樣地，若父親教育年數 FATHEREDUC > 12，則 FATHERCOLL = 1，否則為 0。在此樣本中，有多少比例的父母具有大學教育？

b. 計算 EDUC、MOTHERCOLL 與 FATHERCOLL 之間的相關係數。這些相關係數的大小重要嗎？請提出邏輯論點說明為何 MOTHERCOLL 與 FATHERCOLL 可能比 MOTHEREDUC 與 FATHEREDUC 更適合作為工具變數。

c. 以 MOTHERCOLL 作為工具變數，估計範例 10.5 的工資方程式。EDUC 係數的 95% 區間估計為何？

d. 承 (c) 題，估計第一階段方程式。檢定 MOTHERCOLL 對 EDUC 無影響之虛無假設，其 F 統計量為何？MOTHERCOLL 是否為強工具變數？

e. 以 MOTHERCOLL 與 FATHERCOLL 作為工具變數，估計範例 10.5 的工資方程式。EDUC 係數的 95% 區間估計為何？該區間較 (c) 題之區間 窄還是寬？

f. 承 (e) 題，估計第一階段方程式。檢定 MOTHERCOLL 與 FATHERCOLL 的聯合顯著性。這些工具變數是否足夠強？

g. 就 (e) 題的 IV 估計，檢定額外工具變數的有效性。你得到何結論？


### ANS

**(a) 有部分大學教育的比例（Proportion with Some College）**

- 母親：12.15 %
- 父親：11.68 %

-----

**(b) 相關係數（Correlations）**

<img width="364" alt="未命名" src="https://github.com/user-attachments/assets/26965f10-5705-4cb9-ba17-e077ceb95fd5" />


可以推論，曾經上過大學的父母，對子女教育的重要性會有較高的重視，較之於那些未曾上過大學的父母。


<img width="671" alt="c10q10b" src="https://github.com/user-attachments/assets/22518969-8004-4a8b-95ce-1ce08c3ebff0" />

-----

**(c) 僅使用 MOTHERCOLL 作為工具變數的 2SLS 估計**


<img width="585" alt="未命名" src="https://github.com/user-attachments/assets/1b0a3268-7495-4d59-a3fc-d8db18e261e5" />


<img width="671" alt="c10q18c" src="https://github.com/user-attachments/assets/b07f5586-e8a6-414f-bc2f-bd6e9a93dcd0" />

- 工具變數MOTHERCOLL： 外生、與 EDUC 有相關性
- β^2_EDUC = 0.0760 ： 表示教育每增加一年，工資對數平均增加約 0.076。
- 標準誤差 (SE) = 0.039 ➜ 不小，因此有統計誤差。
- 95% 區間估計係數: [-0.000858, 0.1528939] ➜ 這個區間包含 0，表示在 5% 顯著水準下，這個估計值「不顯著」。

-----

**(d) 第一階段 F 統計量（First-Stage F-Statistic）**

我們利用 第一階段的 F 統計量：𝐹= 工具變數對被解釋變數的解釋力 / 殘差變異

如果這個 F 值夠大，表示這個工具變數對 EDUC 解釋得夠清楚，是「強工具」。

<img width="399" alt="c10q18d" src="https://github.com/user-attachments/assets/3df6846a-f0d0-4847-ad80-8a7aa91b6468" />

這個 F = 63.56 ＞ 10 → 工具變數為強工具（strong instrument）

因此我們拒絕了 IV 。

-----

**(e) 同時使用 MOTHERCOLL 與 FATHERCOLL 的 2SLS 估計**

<img width="578" alt="c10q18e" src="https://github.com/user-attachments/assets/381a360a-c02e-4aaa-8707-4b74630789fe" />

- β^2_EDUC = 0.0878 
- 標準誤差 (SE) = 0.032
- 95% 信賴區間：\([0.027,\;0.148]\)

## 🧩 比較 c 小題與 e 小題：

| 項目 | 單一工具變數（c） | 兩個工具變數（e） |
|------|------------------|------------------|
| β^2_EDUC | 0.0760 | 0.0878 |
| 標準誤 SE | 0.039 | 0.0032 |
| CI | \([-0.000858, 0.1528939]\) | \([0.027, 0.148]\) |
| 顯著性 | 不顯著 | 邊界顯著 |

→ 加入第二個工具變數後，估計值幾乎沒變，但 **標準誤略為下降**、**顯著性變好一些**。

→ 信賴區間較 (c)IV 略窄，表示估計更有效率（higher efficiency）

-----

**(f) 執行 Sargan 檢定（Sargan Test），檢查你在 (e) 小題中所用的兩個工具變數 MOTHERCOLL 和 FATHERCOLL 是否有效**

<img width="466" alt="c10q10f" src="https://github.com/user-attachments/assets/8d79c765-e5c6-4f4d-b39e-71e69c5c53df" />

- \( F = 56.96 \)，p 值 < 0.001  
- 𝐹=56.96>10
- 所以兩個工具變數 MOTHERCOLL 與 FATHERCOLL 在 解釋 EDUC 時非常有力，我們拒絕了這些⼯具較弱的零假設

-----

**(g) 過度識別檢定**

- \( J = 0.214 \)，自由度 \( df = 1 \)
- p 值 = 0.64  
→ 表示額外的工具變數（FATHERCOLL）**有效**

根據第一階段回歸的 F 統計量為 56.96，MOTHERCOLL 和 FATHERCOLL 為強工具變數。然而，第二階段的 Sargan 檢定（𝑛⋅𝑅2=92.43）大於5%臨界值為3.84，拒絕所有工具變數皆有效的虛無假設，顯示至少有一個工具變數與誤差項相關，可能不滿足外生性。

進一步進行 Hausman 檢定時，結果顯示無法拒絕 OLS 與 2SLS 估計值無系統性差異的虛無假設，顯示 EDUC 為外生。故在本案例中，即便存在強工具變數，但考量工具變數之有效性與內生性檢定結果，不建議使用 2SLS 推論，改採 OLS 會更合適。

**SUMMARY TABLE**

回歸結果表（各模型比較）

| 變數         | (1) part (c)        | (2) part (d)        | (3) part (e)        | (4) part (f)        | (5) part (g)        |
|--------------|---------------------|---------------------|---------------------|---------------------|---------------------|
| $C$          | -0.1328 <br> (0.4942) | 12.0791 <br> (0.3031) | -0.2791 <br> (0.3904) | 11.8903 <br> (0.2903) | -0.0001 <br> (0.0954) |
| $EDUC$       | 0.0760 <br> (0.0392) |                     | 0.0878 <br> (0.0306) |                     |                     |
| $EXPER$      | 0.0433 <br> (0.0134) | 0.0562 <br> (0.0421) | 0.0427 <br> (0.0132) | 0.0491 <br> (0.0401) | -0.0001 <br> (0.0132) |
| $EXPER^2$    | -0.0009 <br> (0.0004) | -0.0020 <br> (0.0013) | -0.0008 <br> (0.0004) | -0.0014 <br> (0.0012) | 0.0000 <br> (0.0004) |
| MOTHERCOLL   |                     | 2.5171 <br> (0.3157) |                     | 1.7499 <br> (0.3223) | -0.0442 <br> (0.1060) |
| FATHERCOLL   |                     |                     |                     | 2.1866 <br> (0.3299) | 0.0412 <br> (0.1085) |
| 指標         | (1)      | (2)      | (3)      | (4)      | (5)      |
| $N$          | 428      | 428      | 428      | 428      | 428      |
| $R^2$        | 0.1469781 | 0.1346504 | 0.1529866 | 0.2160603 | 0.0005551 |
| $SSE$        | 190.5032 | 1929.9   | 189.1613 | 1748.339 | 189.0563 |
| $F$          | 21.99178 |          |          | 29.14558 | 0.058735 |

> 註：括號內為標準誤（standard errors）。


-------


### C10Q20

<img width="570" alt="Q20" src="https://github.com/user-attachments/assets/5f18f679-b69e-467f-8e44-8361e5315e43" />


10.20 資本資產定價模型（CAPM）［參見練習 10.14 與 2.16］指出，證券 j 的風險溢酬與市場投資組合的風險溢酬相關：

    r_j − r_f = α_j + β_j (r_m − r_f)

其中 r_j 與 r_f 分別為證券 j 的報酬率與無風險利率；r_m 為市場投資組合報酬率，β_j 為第 j 檔證券的「β 值」。市場投資組合以 標普（S&P）加權指數衡量，無風險利率則以 30 天 LIBOR 月報酬率衡量。如練習 10.14 所述，若市場報酬率帶有測量誤差，則會產生變數測量誤差（errors‑in‑variables）問題。

a. 以資料檔 capm5 中的微軟（Microsoft）觀測值，使用 OLS 估計 CAPM 模型。就此期間而言，您如何分類微軟股票？相對於市場投資組合，是較高風險還是相對安全？

b. 有人提出可透過對解釋變數 (r_m − r_f) 進行排序並以其排名作為工具變數（IV），即將 (r_m − r_f) 由小到大排序，設定 RANK = 1, 2, …, 180。此變數是否滿足 IV1–IV3 條件？請建立 RANK 並取得第一階段迴歸結果。RANK 的係數是否非常顯著？第一階段迴歸的 決定係數 R² 為何？RANK 可視為強工具變數嗎？

c. 計算第一階段殘差 v̂，並將其加入 CAPM 模型，使用 OLS 估計擴充後的方程式。在 1% 顯著水準下檢定 v̂ 的顯著性。是否可以推論市場報酬率為外生？

d. 將 RANK 作為 IV，使用 IV/2SLS 估計 CAPM 模型。比較此 IV 估計與 (a) 題 OLS 估計。IV 估計是否符合您的預期？

e. 建立新變數 POS：若市場報酬率 (r_m − r_f) 為正，則 POS = 1，否則為 0。使用 RANK 與 POS 作為工具變數，取得第一階段迴歸結果。檢定 IV 的聯合顯著性。我們能否認為所選 IV 足夠強？第一階段迴歸的 R² 為何？

f. 利用 (e) 題第一階段方程式的殘差，進行 Hausman 檢定以檢驗內生性。於 1% 顯著水準下，能否認為市場報酬率為外生？

g. 使用 RANK 與 POS 作為工具變數，取得 CAPM 模型的 IV/2SLS 估計。比較此 IV 估計與 (a) 題的 OLS 估計。IV 估計是否符合您的預期？

h. 取 (g) 題的 IV/2SLS 殘差，並（手動）進行 Breusch–Pagan 異質變異數檢定，在 5% 顯著水準下做結論。


### ANS


Microsoft β 值的估計與診斷結果

**(a) 最小平方法 (OLS) 估計**

<img width="570" alt="C10Q20a" src="https://github.com/user-attachments/assets/d6c2af19-415b-4ad2-830e-9cdf84c5e00c" />



模型：

$$
r_{\text{MSFT}} - r_f = 0.0033 + 1.2018 \cdot (r_m - r_f)
$$

估計的 β 值為：

$$
\beta \approx 1.20
$$

$$
R^2 = 0.352
$$

⇒ 95% 區間估計值為 [0.9607882, 1.442891]。
- 根據 OLS 估計結果，Microsoft 股票的 beta 值為 1.2018，顯著高於 1，代表其報酬率變異高於整體市場，屬於「高風險股票」。此外，α>0：表示可能有超額報酬



**(b) 工具變數：RANK**


<img width="575" alt="C10Q20b" src="https://github.com/user-attachments/assets/9689953e-2ae9-4e2b-bff1-8ebb91c4eaef" />




第一階段回歸式：

$$
r_m - r_f = -0.0790 + 0.000907 \cdot \text{RANK}
$$

- t 統計量：43.1  
- $$R^2 = 0.913$$  
⇒ 工具變數非常強，具有高度相關性。



**(c) 外生性檢定（Exogeneity Test）**


<img width="575" alt="C10Q20c" src="https://github.com/user-attachments/assets/bbe0561b-c49f-4c00-a53a-a32141e5b987" />




擴充回歸模型，加入第一階段殘差 $$\hat{v}$$：

- t value −2.04
- p 值：$$0.0428 > 0.01$$  
- 結論：無法在 1% 顯著水準下拒絕 $$r_m - r_f$$ 為外生變數的假設。



**(d) 兩階段最小平方法 (2SLS)：使用 RANK**

<img width="575" alt="C10Q20d" src="https://github.com/user-attachments/assets/2711e36b-d05c-4f60-a7cc-2f901b241f52" />


估計值：

<img width="213" alt="未命名" src="https://github.com/user-attachments/assets/683902a3-baa8-4f3f-94a0-3c5af6b5ff70" />

⇒ 與測量誤差導致 OLS β 低估（attenuation bias）一致。
- 95% 區間估計現在為 [1.028819, 1.527817]。所有值都⼤於 1，因此我們拒絕微軟的 beta 等於 1 的虛無假設。


**(e) 加入第二工具變數：POS**

<img width="576" alt="C10Q20e" src="https://github.com/user-attachments/assets/328bc941-d669-4c57-8ee7-f1ca4c4c50be" />



- 第一階段 F 統計量：$$F = 951.3$$  
- $$R^2 = 0.915$$  
⇒ 工具變數整體非常強。
- F>10，且⼤於使⽤測試規模標準的Stock-Yogo臨界值19.93。如果接受 5% 測試的 10% 測試 I 型錯誤，得出結論，IV 並不弱。


**(f) Hausman 檢定**

<img width="577" alt="C10Q20f" src="https://github.com/user-attachments/assets/0d1962a1-296d-4780-bf12-589feb2e46e5" />



- 殘差 p 值：$$0.0287 > 0.01$$  
- 結論：無法在 1% 顯著水準下拒絕外生性假設。



**(g) 兩階段最小平方法：使用 RANK 與 POS**


<img width="575" alt="C10Q20g" src="https://github.com/user-attachments/assets/df5ed4a9-5de1-4841-a6a0-2e60d9c6b78d" />




估計值：

<img width="242" alt="未命名" src="https://github.com/user-attachments/assets/bcfb7702-0486-4fe6-9503-982dfbb460f3" />


- 與 (d) 的結果相近，且皆大於 OLS。
- 係數估計值 1.2866 ⼤於部分 (a) 中的 OLS 估計值。 95% 區間估計為 [1.037005, 1.536111]。所有值都⼤於 1，因此拒絕微軟的 beta 等於 1 的虛無假設


**(h) Breusch–Pagan 異質變異檢定**


<img width="362" alt="C10Q20h" src="https://github.com/user-attachments/assets/8a65e2b1-8678-4239-b7fc-74e7f17d4988" />




$$
\chi^2(1) = 1.57,\quad p = 0.21
$$

-NR2= 63.03 ⼤於 99⽇χ的百分位數即 6.635，在 5% 顯著水準下無異質變異的證據。因此我們拒絕盈餘IV的有效性。我們得出結論，肯定是出了問題，必須尋找新的 IV。



**SUMMARY TABLE**


回歸結果表（第二組模型）

**第一段回歸結果**

| 變數     | (1) part (a)         | (2) part (b)          | (3) part (c)         | (4) part (d)         |
|----------|----------------------|------------------------|----------------------|----------------------|
| $C$      | 0.0032 <br> (0.54)    | -0.0790 <br> (-36.00)  | 0.0030 <br> (0.50)   | 0.0030 <br> (0.50)   |
| $MKTRRET$| 1.2018 <br> (9.84)    |                        | 1.2783 <br> (10.09)  | 1.2783 <br> (10.04)  |
| $RANK$   |                      | 0.0009 <br> (43.10)    |                      |                      |
| $VHAT$   |                      |                        |                      | -0.8746 <br> (-2.04) |
| 指標     | (1)       | (2)       | (3)       | (4)       |
| $N$      | 180       | 180       | 180       | 180       |
| $R^2$    | 0.3522665 | 0.9125559 | 0.3671528 | 0.35084   |
| $SSE$    | 1.162878  | 0.038266  | 1.136153  | 1.165439  |
| $RMSE$   | 0.0808271 | 0.0146661 | 0.0801183 | 0.0804653 |

---

**第二段回歸結果**

| 變數     | (1) part (e)           | (2) part (f)          | (3) part (g)         | (4) part (h)         |
|----------|------------------------|------------------------|----------------------|----------------------|
| $C$      | -0.0791 <br> (-35.88)   | 0.0030 <br> (0.50)     | 0.0030 <br> (0.50)   | -0.0042 <br> (-0.43) |
| $MKTRRET$|                        | 1.2866 <br> (10.18)    | 1.2866 <br> (10.10)  |                      |
| $RANK$   | 0.0009 <br> (35.47)     |                        |                      | -0.0006 <br> (-5.52) |
| $POS$    | 0.0009 <br> (0.35)      |                        |                      | 0.1150 <br> (9.77)   |
| $VHAT2$  |                        |                        | -0.9695 <br> (-2.27) |                      |
| 指標     | (1)       | (2)       | (3)       | (4)       |
| $N$      | 180       | 180       | 180       | 180       |
| $R^2$    | 0.9126166 | 0.370547  | 0.3505161 | 0.3501788 |
| $SSE$    | 0.03826   | 1.130059  | 1.166021  | 0.7577049 |
| $RMSE$   | 0.0147023 | 0.0799032 | 0.0804854 | 0.065428  |

> 註：括號內數值為 t 值。



------------


### C10Q24

<img width="664" alt="Q24" src="https://github.com/user-attachments/assets/23b4a0f6-ca7e-4576-b155-83dceb96d874" />


10.24 請考慮 working wives 資料檔 mroz，使用 428 位參與勞動市場的已婚婦女觀測值。本練習旨在檢驗替代標準誤對 IV 估計量之效果。請以 IV/2SLS 方法，使用 MOTHEREDUC 與 FATHEREDUC 作為工具變數，估計範例 10.5 的模型，並將結果視為基準模型。

a. 計算 IV/2SLS 殘差 ê_IV，並將其與 EXPER 作圖。殘差是否呈現符合同質變異 (homoskedasticity) 的型態？

b. 將 ê_IV² 對常數項與 EXPER 進行迴歸，並套用第 8 章的 NR² 檢定，以檢驗是否存在異質變異數。

c. 在軟體中選擇異質變異數穩健標準誤 (Heteroskedasticity Robust Standard Errors) 選項，取得 IV/2SLS 估計。穩健標準誤相較於基準模型的標準誤是變大還是變小？請以穩健標準誤計算 EDUC 係數的 95% 區間估計。

d. 以 B = 200 次自助抽樣 (bootstrap) 重新抽樣，並在軟體中選擇自助抽樣標準誤 (Bootstrap standard errors) 選項，取得 IV/2SLS 估計。自助抽樣標準誤相較於基準模型的標準誤是變大還是變小？與 (c) 題中的異質變異數穩健標準誤相比如何？請以自助抽樣標準誤計算 EDUC 係數的 95% 區間估計。

### ANS

**(a)殘差圖**

<img width="588" alt="1" src="https://github.com/user-attachments/assets/bbdb4c67-38c6-49d0-97b8-af02e4857aca" />

將 IV 殘差（IV residuals）對經驗值（EXPER）繪圖後，顯示出明顯的漏斗型（funnel pattern）分佈，顯示可能存在異質變異（heteroskedasticity）的情形，說明經驗年資較低時的殘差變化⽐經驗年限較⾼時的殘差變化更⼤。
 




**(b)** NR²（Breusch–Pagan）檢定

將殘差平方對常數項與 `EXPER` 進行迴歸：

$$
R^{2} = 0.0174,\quad NR^{2} = 428 \times 0.0174 \approx 7.44
$$

臨界值為：

$$
\chi^2_{0.95}(1) = 3.84
$$

因為 $7.44 > 3.84$（p 值約為 0.006），**拒絕同質變異（homoskedasticity）假設**，表示存在**異質變異（heteroskedasticity）**。






**(c)** HC0（White）穩健標準誤

| 係數項 | 傳統標準誤 | HC0 穩健標準誤 | 95% 信賴區間（HC0）        |
|--------|--------------|------------------|-----------------------------|
| EDUC   | 0.0333       | 0.0350           | $[-0.004,\ 0.127]$          |

穩健標準誤大於IV標準誤，且信賴區間包含 0，表示在 5% 顯著水準下，**EDUC 係數不顯著**。

使⽤穩健標準誤差對係數的區間估計是[-0.004, 0.127]。IV 估計標準誤給出區間估計是 [−0.0004, 0.123]






**(d)** 自助法（Bootstrap, B = 200）

| 係數項 | Bootstrap 標準誤 | 95% 信賴區間（Bootstrap）      |
|--------|------------------|-------------------------------|
| EDUC   | 0.0338           | $\left[-0.003,\ 0.126\right]$ |

Bootstrap 標準誤介於傳統與 HC0 值之間，所產生的信賴區間幾乎與 HC0 相同，亦涵蓋 0。

Bootstrap⽐穩健標準誤差稍⼩，但仍⽐IV 標準誤差稍⼤。區間估計為[-0.003,0.126]。





**SUMMARY TABLE**

<img width="611" alt="c10q24a" src="https://github.com/user-attachments/assets/53312e3d-c95a-474f-b701-75aabab68940" />



