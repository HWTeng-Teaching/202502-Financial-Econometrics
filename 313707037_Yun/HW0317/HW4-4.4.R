#a.b.
# 設定經驗值範圍
exper1 <- 0:30  # Model 1 的範圍
exper2 <- 1:30  # Model 2 不能包含 0

# 計算對應的 RATING 值
rating_model1 <- 64.289 + 0.990 * exper1
rating_model2 <- 39.464 + 15.312 * log(exper2)

# 繪製 Model 1 (藍線)
plot(exper1, rating_model1, type="l", col="blue", lwd=2, ylim=c(60, 100),
     xlab="Years of Experience", ylab="Predicted RATING",
     main="Fitted Values of Model 1 and Model 2")

# 繪製 Model 2 (紅線)
lines(exper2, rating_model2, col="red", lwd=2)

# 加上圖例
legend("bottomright", legend=c("Model 1 (Linear)", "Model 2 (Log)"), 
       col=c("blue", "red"), lwd=2)


#c.d.
marginal_effect_model1 <- 0.990
marginal_effect_model1
marginal_effect_10 <- 15.312 / 10
marginal_effect_20 <- 15.312 / 20

marginal_effect_10
marginal_effect_20