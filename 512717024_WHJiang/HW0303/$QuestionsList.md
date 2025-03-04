
## C02Q17 a ~ g (Computer Exercise)
![C02Q17](https://github.com/user-attachments/assets/23ae9b10-d623-4468-9c54-831dfc437d55)

## C02Q25 a ~ f (Computer Exercise)
![C02Q25](https://github.com/user-attachments/assets/d0ea2c24-0947-4b11-92e5-dbe352c94670)

## C02Q28 a ~ f (Computer Exercise)
![C02Q28](https://github.com/user-attachments/assets/ab9cdd15-921b-41f2-8935-346e18d6d87c)

## C02Q17 a ~ g (Computer Exercise)
### (分析步驟)

## (a) 繪圖部分
使用 `plot()` 函數繪製房價 (PRICE) 與房屋面積 (SQFT) 的散佈圖，其中 x 軸表示房屋面積（單位：百平方英尺），y 軸表示房價（單位：千美元）。

## (b) 線性迴歸模型
使用 `lm(PRICE ~ SQFT, data = collegetown)` 估計線性迴歸模型，並利用 `summary()` 檢視估計結果。隨後使用 `abline()` 函數在散佈圖上加上紅色的擬合直線。解釋中說明了斜率 β₂ 的意義（每增加100平方英尺對房價的影響）及截距 β₁。

## (c) 二次迴歸模型
使用 `lm(PRICE ~ I(SQFT^2), data = collegetown)` 估計僅含平方項的二次模型。然後計算在 SQFT = 20 (即2000平方英尺) 處的邊際效應，該效應為導數  
dPRICE/dSQFT = 2α₂SQFT_target。

## (d) 擬合曲線與切線
產生一個連續的 SQFT 序列，並利用 `predict()` 計算二次模型的預測值。接著，繪製擬合曲線和資料點，並根據在 SQFT = 20 處計算的切線斜率繪製出綠色虛線表示切線。圖例標示不同曲線。

## (e) 彈性計算
利用切線斜率（即邊際效應）和該點的房屋面積與預測價格計算彈性：  
Elasticity = (dPRICE/dSQFT) * (SQFT/PRICE)。

## (f) 殘差分析
分別計算線性與二次模型的殘差，並使用並列圖 (`par(mfrow=c(1,2))`) 作圖，檢查殘差是否存在系統性模式或異方差性問題。

## (g) SSE 比較
分別計算兩模型的殘差平方和 (SSE)，並比較哪個模型的 SSE 較低，從而判斷哪個模型擬合效果較好。

