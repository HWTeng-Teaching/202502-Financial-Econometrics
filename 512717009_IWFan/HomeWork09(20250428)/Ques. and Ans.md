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



------------


### C10Q24

<img width="664" alt="Q24" src="https://github.com/user-attachments/assets/23b4a0f6-ca7e-4576-b155-83dceb96d874" />


10.24 請考慮 working wives 資料檔 mroz，使用 428 位參與勞動市場的已婚婦女觀測值。本練習旨在檢驗替代標準誤對 IV 估計量之效果。請以 IV/2SLS 方法，使用 MOTHEREDUC 與 FATHEREDUC 作為工具變數，估計範例 10.5 的模型，並將結果視為基準模型。

a. 計算 IV/2SLS 殘差 ê_IV，並將其與 EXPER 作圖。殘差是否呈現符合同質變異 (homoskedasticity) 的型態？

b. 將 ê_IV² 對常數項與 EXPER 進行迴歸，並套用第 8 章的 NR² 檢定，以檢驗是否存在異質變異數。

c. 在軟體中選擇異質變異數穩健標準誤 (Heteroskedasticity Robust Standard Errors) 選項，取得 IV/2SLS 估計。穩健標準誤相較於基準模型的標準誤是變大還是變小？請以穩健標準誤計算 EDUC 係數的 95% 區間估計。

d. 以 B = 200 次 (bootstrap) 重新抽樣，並在軟體中選擇抽樣標準誤 (Bootstrap standard errors) 選項，取得 IV/2SLS 估計。抽樣標準誤相較於基準模型的標準誤是變大還是變小？與 (c) 題中的異質變異數穩健標準誤相比如何？請以抽樣標準誤計算 EDUC 係數的 95% 區間估計。








