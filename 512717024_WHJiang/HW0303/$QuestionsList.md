
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

## C02Q25 a ~ f (Computer Exercise)
# 分析步驟

## (a) 描述性統計與直方圖
使用 `hist()` 函數繪製 `FOODAWAY` 的直方圖，同時計算平均值、中位數，以及第25與第75百分位數。這些統計量有助於描述資料的集中趨勢與離散程度。

## (b) 分組計算
根據家庭中成員的學歷狀況（假設變數名稱為 "degree"），利用 `dplyr` 套件分群計算 `FOODAWAY` 的平均值與中位數。如果資料中不存在此變數，則需根據實際情況調整程式碼。

## (c) 取自然對數與描述性統計
將 `FOODAWAY` 取自然對數後繪製直方圖並計算描述性統計數據。由於 `log()` 僅對正數有意義，非正值會被轉換為 NA，因此 `ln(FOODAWAY)` 的觀察數可能少於 `FOODAWAY`。

## (d) 線性迴歸模型估計
利用 `lm()` 函數估計模型  
`ln(FOODAWAY) = β₁ + β₂ INCOME + e`  
這裡 β₂ 的估計值表示當 `INCOME` 增加 1 單位（即100美元）時，`ln(FOODAWAY)` 的變化量，進而可近似解釋為 `FOODAWAY` 的百分比變化。

## (e) 散佈圖與擬合直線
繪製 `ln(FOODAWAY)` 與 `INCOME` 的散佈圖，並用 `abline()` 加上模型 (d) 的擬合直線，使我們能視覺化 `INCOME` 對 `ln(FOODAWAY)` 的影響。

## (f) 殘差分析
計算模型 (d) 的最小平方法殘差，並將殘差對 `INCOME` 作圖。殘差圖可以幫助檢查模型假設是否成立，例如是否存在異方差性或其他非隨機模式。

