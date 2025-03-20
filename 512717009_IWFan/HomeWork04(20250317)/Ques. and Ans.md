## Q.04

![image](https://github.com/user-attachments/assets/e7f650d6-638c-4560-84dd-4bbdd6e2e19c)

## Ans:
**(a)**
Model 1: 擬合值計算

模型 1 的方程式為：RATING = 64.289 + 0.990 × EXPER

![image](https://github.com/user-attachments/assets/bcbe5d55-fc54-4455-ab90-385821e38d16)

![image](https://github.com/user-attachments/assets/5fb7930d-60e1-4119-b261-3e6ed200f87b)

Model 1（線性回歸） 顯示評分隨經驗線性增加。

--------------------------------------------------------------

**(b)**
模型 2 的方程式為：

RATING = 39.464 + 15.312 ln(EXPER)

![image](https://github.com/user-attachments/assets/fa5a35a0-e9d4-446a-979d-aa56e3b3a00b)

![image](https://github.com/user-attachments/assets/d4879e47-6838-43ce-a05b-555ca43d3542)

Model 2（對數回歸） 顯示早期經驗的影響較大，但隨著時間推移邊際效益遞減。

--------------------------------------------------------------

**(c)**
Model 1: 邊際效應

模型 1 為線性模型，邊際效應為固定值：d(RATING) / d(EXPER) = 0.990

Model 1（線性回歸）：邊際影響為 0.990，固定不變，表示無論經驗多少，每增加一年經驗，評分都會提高 0.990。因此，無論藝術家有多少經驗，額外一年的影響皆為 0.990。

![image](https://github.com/user-attachments/assets/1a2fd9e9-8b3d-4dfe-8b88-1fbfa7403edd)

--------------------------------------------------------------

**(d)**
模型 2 的邊際效應計算如下：d(RATING) / d(EXPER) = 15.312 / EXPER

![image](https://github.com/user-attachments/assets/61f2ebca-1462-49ea-99d4-900e5eacfba2)

Model 2（對數回歸）：

10 年經驗時，邊際影響為 1.5312，表示經驗增加一年，評分提高較多。

20 年經驗時，邊際影響下降至 0.7656，顯示邊際效益遞減，符合學習曲線理論。

這表明當經驗值增加時，額外一年經驗的影響會逐漸減少。

![image](https://github.com/user-attachments/assets/93feffbb-05f8-4c8a-8454-f4d7a1b72b6e)

![image](https://github.com/user-attachments/assets/5b94a26c-6b9d-4904-897c-512a26189f87)

--------------------------------------------------------------

**(e)**

![image](https://github.com/user-attachments/assets/80f5d53a-5b70-47c7-9cbf-1f983b71cfb1)

根據 𝑅2值比較：

Model 1（所有數據）： 𝑅2=0.3793（擬合效果較差）。

Model 1（僅有經驗者）：𝑅2=0.4858（稍有改善，但仍不如 Model 2）。

Model 2（對數回歸）：𝑅2=0.6414（擬合效果最佳）。

--------------------------------------------------------------

**(f)**

Model 1 假設每年經驗對評分的影響固定，適用於經驗少的個體，但不能解釋隨時間變長後的邊際變化，這在現實中不太合理。

Model 2 使用對數形式，擬合數據較好，因為它解釋了 64.14% 的變異，顯示邊際效應遞減，這與學習曲線概念相符。

因此，Model 2比 Model 1 適合度更高，更符合經濟學概念（學習曲線）。

## Q.28

![image](https://github.com/user-attachments/assets/5a5acfca-8a2e-41fe-bfbc-777971f0fceb)

## Ans:
**(a)**
我們考慮以下四種回歸模型來擬合 Northampton Shire 小麥產量數據：

1. 線性模型 (Linear Model): YIELD_t = β0 + β1 TIME + e_t
2. 
3. 對數模型 (Logarithmic Model): YIELD_t = α0 + α1 ln(TIME) + e_t
4. 
5. 二次模型 (Quadratic Model): YIELD_t = γ0 + γ1 TIME² + e_t
6. 
7. 對數線性模型 (log-linear  Model): ln(YIELD_t) = φ0 + φ1 TIME + e_t
8. 
模型回歸結果：
![image](https://github.com/user-attachments/assets/9c9e407f-c7a1-49f6-8145-5de3bf6cb22a)

![image](https://github.com/user-attachments/assets/21f50294-6136-42bb-8079-0ba168d60580)

![image](https://github.com/user-attachments/assets/59f0317f-d35b-4ba5-86ee-39a1615b228f)

![image](https://github.com/user-attachments/assets/2ced41f4-8224-4269-9635-905050b466b3)

Summary Table：
![image](https://github.com/user-attachments/assets/663534e7-1a67-4e01-9d81-b0c192c340e9)

(i) 擬合曲線比較
![image](https://github.com/user-attachments/assets/ef62e29f-a005-45b4-9b6a-c3753762898b)

![image](https://github.com/user-attachments/assets/e39599d0-a36e-40f1-9acb-726272ca3067)

顯示了四個回歸模型的擬合曲線，與實際數據進行比較：

藍色虛線：線性模型（Linear Fit）適用於較長期的趨勢，但可能低估了早期或後期的變化。

紅色虛線：對數模型（Logarithmic Fit）適用於初期增長較快的情況，但在後期可能低估變化。

綠色虛線：二次模型（Quadratic Fit）具有較好的擬合效果，因為它允許加速度變化。

紫色虛線：對數線性模型（Long-linear  Fit）在後期的預測能力可能較強。

ii) 殘差圖
![image](https://github.com/user-attachments/assets/6f47f002-4316-4de4-99bf-ced54bd15399)

這四張殘差圖顯示了不同模型的殘差分布情況：

線性模型（Linear Model）：殘差顯示出某種曲線模式，表明線性模型可能低估了某些時間段的變化。

對數模型（Logarithmic Model）：在早期數據（低 TIME 值）中，殘差較大，表示該模型可能高估了早期的產量。

二次模型（Quadratic Model）：殘差分布較為隨機，沒有明顯的模式，這表示該模型可能適配較好。

對數線性模型（Long-linear Model）：殘差在某些區域內仍然存在系統性偏差，表明該模型可能不完全適合。

基於殘差圖，我們可以初步判斷 二次模型（Quadratic Model）較適合，⼆次模型具有更⾼的R2殘差圖沒有顯⽰中⼼區域的明顯下降。

(iii) 誤差的常態性檢驗
![image](https://github.com/user-attachments/assets/955c30a7-e192-4679-98ca-eea36f225211)

這些 Q-Q 圖與 Shapiro-Wilk 正態性檢驗結果顯示：

線性模型（Linear Model）：殘差較接近正態分布，p-value = 0.679（未拒絕正態性）

對數模型（Logarithmic Model）：殘差有些偏離正態，p-value = 0.186（未拒絕正態性）

二次模型（Quadratic Model）：殘差最接近正態分布，p-value = 0.827（未拒絕正態性）

對數線性模型（Long-linear Model）：殘差偏離正態分布，p-value = 0.00007（顯著拒絕正態性）

(iv) R² 值

綜合擬合曲線、殘差圖與正態性檢驗，我們可以選擇**二次模型（Quadratic Model）**作為最佳模型，因為：

它的 𝑅2最高（0.689），表示擬合效果最佳。

它的殘差分布較隨機，沒有明顯的系統性模式。

它的殘差符合正態性（p-value = 0.827）。

--------------------------------------------------------------

**(b)**
對於⼆次模型，估計係數為 0.0004986，邊際效應為2(0.0004986)時間。因此，斜率在每個時間點都會改變。評估邊際效應時間=10、20、30 和 40，邊際效應屈服估計為0.0099724、0.0199447、0.0299171和0.0398894。隨著時間的推移，技術的累積效應會使產量不斷增加，因此邊際效應也會隨之增加。

--------------------------------------------------------------

**(c)**
使用 學生化殘差（Studentized Residuals）、Leverage, DFFITS, DFBETAS 來檢測異常觀測值

![image](https://github.com/user-attachments/assets/513788ee-3b68-4fd9-96ab-c04a0e097a7d)

上方圖表與表格顯示了異常觀測值分析的結果：

學生化殘差（Studentized Residuals）= 2

如果殘差的絕對值超過 2，則該觀測值可能是異常值。

![image](https://github.com/user-attachments/assets/d656eaf7-348d-4e04-afdc-98e906bf7328)

Leverage（槓桿值）= 0.0833

![image](https://github.com/user-attachments/assets/322828f5-2d2d-4e71-a342-26cd685bc8a1)

高槓桿值點（如 1996-1997 年）表示這些觀測值對回歸影響較大。

在 TIME 變數較大的區間（40 年後），槓桿值顯著上升。

DFBETAS = 0.2887 

衡量個別觀測值對 TIME_SQ 變數的回歸係數影響。如果絕對值超過 2/sqrt(n)，則該觀測值可能是影響較大的異常值。

![image](https://github.com/user-attachments/assets/e1fc6fcc-4f35-4ce0-9181-d5b956c8c3f4)

DFFITS（影響分數）= 0.2041

![image](https://github.com/user-attachments/assets/7ea83454-7a34-4541-bf5a-fed0aaf729bb)

異常高的 DFFITS 表示某些點可能極大影響回歸結果。

1992 年（TIME=43）的影響分數最高，顯示它可能是影響回歸的異常值。

異常觀測值表：

1963 年（TIME=14, YIELD=0.3024）是低產量的異常點，對回歸影響較大。

1992 年（TIME=43, YIELD=2.3161）是產量異常高的點，影響回歸係數顯著。

這些異常點可能是由極端天氣、政策變化或技術突破所導致的。異常值檢測顯示 1963（低產量）與 1992（高產量）影響較大

--------------------------------------------------------------

**(d)**

我們使用二次模型來預測 1997 年的產量，並計算 95% 信賴區間。

- **1997 年預測產量**: 2.2318 噸/公頃
- 
- **95% 信賴區間**: [1.3724, 2.3898]
- 
這表示我們有 95% 的信心，1997 年的預測值落在合理的 95% 信賴區間內。










