#a.
# 經驗值範圍
EXPER1 <- 0:30

# Model 1 fitted values
RATING1 <- 64.289 + 0.990 * EXPER1

# 繪圖
plot(EXPER1, RATING1, type="l", col="blue", lwd=2,
     xlab="Years of Experience (EXPER)",
     ylab="Fitted Rating",
     main="Model 1: Fitted Rating vs Experience")

#b.

# 經驗值範圍
EXPER2 <- 1:30

# Model 2 fitted values
RATING2 <- 39.464 + 15.312 * log(EXPER2)

# 繪圖
plot(EXPER2, RATING2, type="l", col="red", lwd=2,
     xlab="Years of Experience (EXPER)",
     ylab="Fitted Rating",
     main="Model 2: Fitted Rating vs Experience")