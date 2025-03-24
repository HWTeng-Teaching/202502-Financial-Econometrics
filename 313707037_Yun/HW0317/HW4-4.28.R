#a.
url <- "https://www.principlesofeconometrics.com/poe5/data/ascii/wa_wheat.dat"
download.file(url, destfile = "wa_wheat.dat")
DATA <- read.table("wa_wheat.dat", header = FALSE)
colnames(DATA) <- c("NORTHAMPTON","CHAPMAN","MULLEWA","GREENOUGH","TIME")
head(DATA)  
# 建立四種回歸模型
MODEL1 <- lm(NORTHAMPTON ~ TIME, data=DATA)
MODEL2 <- lm(NORTHAMPTON ~ log(TIME), data=DATA)
MODEL3 <- lm(NORTHAMPTON ~ I(TIME^2), data=DATA)
MODEL4 <- lm(log(NORTHAMPTON) ~ TIME, data=DATA)

# 模型摘要
summary(MODEL1)
summary(MODEL2)
summary(MODEL3)
summary(MODEL4)

# 繪製擬合值與原始數據
plot(DATA$TIME, DATA$NORTHAMPTON, main="Fitted Models Comparison", xlab="TIME", ylab="YIELD")
lines(DATA$TIME, fitted(MODEL1), col="blue", lwd=2)  # 線性模型
lines(DATA$TIME, fitted(MODEL2), col="red", lwd=2)   # 對數模型
lines(DATA$TIME, fitted(MODEL3), col="green", lwd=2) # 二次模型
lines(DATA$TIME, exp(fitted(MODEL4)), col="purple", lwd=2) # 指數模型

# 加上圖例
legend("topleft", legend=c("MODEL1", "MODEL2", "MODEL3", "MODEL4"),
       col=c("blue", "red", "green", "purple"), lwd=1)


#a-1) 殘差圖 (Residuals Plot)

# 設定畫面為 2x2，方便一次畫四個模型的殘差圖
par(mfrow=c(2,2))

# 殘差圖
plot(fitted(MODEL1), residuals(MODEL1), main="Residuals Plot (LINEAR)", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red", lwd=2)

plot(fitted(MODEL2), residuals(MODEL2), main="Residuals Plot (LOG)", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red", lwd=2)

plot(fitted(MODEL3), residuals(MODEL3), main="Residuals Plot (QUADRATIC)", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red", lwd=2)

plot(fitted(MODEL4), residuals(MODEL4), main="Residuals Plot (EXPONENTIAL)", xlab="Fitted Values", ylab="Residuals")
abline(h=0, col="red", lwd=2)


#(a-2) 殘差常態性檢定 (Error Normality Test)

# 設定畫面為 2x2
par(mfrow=c(2,2))

# QQ-Plot 檢查殘差是否符合常態分佈
qqnorm(residuals(MODEL1), main="QQ-Plot (LINEAR)")
qqline(residuals(MODEL1), col="red", lwd=2)

qqnorm(residuals(MODEL2), main="QQ-Plot (LOG)")
qqline(residuals(MODEL2), col="red", lwd=2)

qqnorm(residuals(MODEL3), main="QQ-Plot (QUADRATIC)")
qqline(residuals(MODEL3), col="red", lwd=2)

qqnorm(residuals(MODEL4), main="QQ-Plot (EXPONENTIAL)")
qqline(residuals(MODEL4), col="red", lwd=2)

# Shapiro-Wilk 正態性檢定 (p < 0.05 表示殘差不符合常態)
shapiro.test(residuals(MODEL1))
shapiro.test(residuals(MODEL2))
shapiro.test(residuals(MODEL3))
shapiro.test(residuals(MODEL4))


#b.

summary(MODEL1)


#c.

# 計算異常值診斷指標
DATA$STD_RESIDUALS <- rstandard(MODEL3)  # Studentized Residuals
DATA$LEVERAGE <- hatvalues(MODEL3)       # Leverage
DATA$DFBETAS <- dfbetas(MODEL3)          # DFBETAS
DATA$DFFITS <- dffits(MODEL3)            # DFFITS

# 繪製異常值診斷圖
par(mfrow=c(2,2))
plot(DATA$TIME, DATA$STD_RESIDUALS, main="STUDENTIZED RESIDUALS", xlab="TIME", ylab="RESIDUALS")
plot(DATA$TIME, DATA$LEVERAGE, main="LEVERAGE", xlab="TIME", ylab="LEVERAGE")
plot(DATA$TIME, DATA$DFFITS, main="DFFITS", xlab="TIME", ylab="DFFITS")
plot(DATA$TIME, DATA$DFBETAS[,2], main="DFBETAS (TIME)", xlab="TIME", ylab="DFBETAS")