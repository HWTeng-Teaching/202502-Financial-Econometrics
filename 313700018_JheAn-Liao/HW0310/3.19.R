rm(list = ls())
# 🌟 載入必要套件
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# 🔗 下載並載入 motel 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(motel)

#A.
# 設定畫布範圍 (ylim) 時，確保能同時容納兩組資料
plot(motel$time, motel$motel_pct,
     type = "l",               # 折線圖
     col = "blue",
     ylim = range(c(motel$motel_pct, 120)),
     xlab = "Time (Month Index)",
     ylab = "Occupancy Rate (%)",
     main = "Motel vs. Competitor Occupancy Rates Over Time")

# 在同一張圖上再加上競爭者的住房率折線
lines(motel$time, motel$comp_pct, col = "red")

# 加上圖例
legend("topleft",
       legend = c("Motel Occupancy", "Competitor Occupancy"),
       col = c("blue", "red"),
       lty = 1)
# 執行線性迴歸
model <- lm(motel_pct ~ comp_pct, data = motel)

# 查看迴歸結果
summary(model)
confint(model, level = 0.95)

#從圖中可見，多數月份中 motel 的住房率（藍線）
#大部分時間都高於 competitor 的住房率（紅線），但仍有幾個月份兩者接近或 competitor 略高。

#兩者是否同向波動？
#雖然曲線不完全重疊，但大體上隨著時間變化，兩者確實有一定程度的同向變動
#（例如在某些月份同時上升或下降）。這也在後續的迴歸分析中得到部分印證。
#迴歸結果顯示住房率成正相關

#B.
# 建立一個新資料框，指定 comp_pct = 70
newdata <- data.frame(comp_pct = 70)

# 以 90% 信賴水準預測 
predict(model, newdata = newdata, 
        interval = "confidence", 
        level = 0.90)

#E.
model <- lm(motel_pct ~ comp_pct, data = motel)
resid_model <- resid(model)
#作圖
plot(motel$time, resid_model,
     type = "p",           # "b" 表示點與線都畫出
     pch = 16, col = "blue",
     xlab = "Time (Month Index)",
     ylab = "Residuals",
     main = "Residuals vs. Time")

abline(h = 0, col = "red", lwd = 2)  # 參考線，表示殘差=0
#殘差在前半段（約 time=1～10）多為正值，表示在這些月份中，
#實際的 motel_pct 高於 模型預測值。
#在後半段（尤其是 time=17 之後），殘差多為負值，表示在這些月份中，
#實際的 motel_pct 低於 模型預測值。
#這種「早期殘差普遍為正，後期殘差普遍為負」的現象，
#顯示模型對不同時期可能存在系統性高估或低估的情況。






