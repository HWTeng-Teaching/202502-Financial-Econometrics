#4.4
#a
m1 <- function(x){y=64.289+0.990*x}
curve(m1,0,30,lwd=2,col='blue',xlab = 'EXPER',ylab = 'RATING',main='Model 1')
x_points <- c(0, 30)
y_points <- m1(x_points)
points(x_points, y_points, pch=19, col='red')
text(x_points, y_points, labels = paste0("(", x_points, ", ", round(y_points, 4), ")"), pos=c(4,2), cex=0.8)

#b
m2 <- function(x){y=39.464+15.312*log(x)}
curve(m2,1,30,lwd=2,col='green',xlab = 'EXPER',ylab = 'RATING',main='Model 2')
x2_points <- c(1, 30)
y2_points <- m2(x2_points)
points(x2_points, y2_points, pch=19, col='red')
text(x2_points, y2_points, labels = paste0("(", x2_points, ", ", round(y2_points, 4), ")"), pos=c(4,2), cex=0.8)

#4.28
#a
TIME <- wa_wheat$time
YIELD <- wa_wheat$northampton

model1 <- lm(YIELD ~ TIME)
model2 <- lm(YIELD ~ log(TIME))
model3 <- lm(YIELD ~ I(TIME^2))
model4 <- lm(log(YIELD) ~ TIME)

model_list <- list(model1, model2, model3, model4)
titles <- c("Model 1: YIELD ~ TIME",
            "Model 2: YIELD ~ log(TIME)",
            "Model 3: YIELD ~ TIME^2",
            "Model 4: log(YIELD) ~ TIME")

for (i in 1:4) {
  cat("\n---", titles[i], "---\n")
  print(summary(model_list[[i]]))
}
#i
plot(TIME,YIELD,pch=16,cex=0.7,xlab = 'TIME',ylab = 'YIELD',ylim = c(-0.5,2.5))
lines(TIME, fitted(model1), col = "red", lwd = 2)
lines(TIME, fitted(model2), col = "blue", lwd = 2)
lines(TIME, fitted(model3), col = "green", lwd = 2)
lines(TIME, fitted(model4), col = "orange", lwd = 2)
legend("topleft",legend = c("Model 1: YIELD ~ TIME",
                            "Model 2: YIELD ~ log(TIME)",
                            "Model 3: YIELD ~ TIME^2",
                            "Model 4: log(YIELD) ~ TIME"),
       col = c("red", "blue", "green", "orange"),lwd = 2,cex = 0.8,bty = "n")
#ii
par(mfrow = c(2, 2))
for (i in 1:4) {
  plot(TIME, resid(model_list[[i]]), pch = 16, cex = 0.7, 
       xlab = "TIME", ylab = "Residuals", 
       main = titles[i])
  abline(h = 0, col = "red", lwd = 2)
}

#iii
par(mfrow = c(2, 2))
for (i in 1:4) {
  res <- resid(model_list[[i]])
  qqnorm(res, main = paste(titles[i], "- Q-Q Plot"), pch = 16, cex = 0.7)
  qqline(res, col = "red", lwd = 1)
}

#b
best_model <- model3
summary(best_model)$coefficients

#c
studentized_residuals <- rstudent(best_model)
plot(1:48,studentized_residuals ,xlab = 'INDEX',main = 'Studentized Residuals')
abline(h =c(-2,2), col = "red", lwd=2) #95%C.I.

leverage_values <- hatvalues(best_model)
plot(1:48,leverage_values,xlab = 'INDEX',main = 'LEVERAGE')
abline(h =2*2/48, col = "red", lwd=2) #h_bar=2/48

plot(dffits(best_model), main = "DFFITS")
abline(h=2*sqrt(2/48),col='red',lwd=2)
abline(h=-2*sqrt(2/48),col='red',lwd=2)

dfbetas_plot <- dfbetas(best_model)
matplot(dfbetas_plot, type = "h", main = "DFBETAS")
abline(h=2/sqrt(48),col='red',lwd=2)
abline(h=-2/sqrt(48),col='red',lwd=2)

#d
train <- wa_wheat[1:47, ]
model_train <- lm(northampton ~ I(time^2), data=train)
newdata <- data.frame(time = 48)
(pred <- predict(model_train, newdata, interval = "prediction",
                 level = 0.95))
(origin <- wa_wheat[48, 1])

#4.29
#a
data('cex5_small')
dataa <- cex5_small[,c(6,9)]
summary(dataa)
sapply(dataa, sd)

par(mfrow = c(1, 2))
for(i in 1:2){
  hist(dataa[,i],main = colnames(dataa)[i],xlab = colnames(dataa)[i],breaks = 40)
  abline(v = mean(dataa[,i]), col = "red", lwd = 2, lty = 2)   # 紅色虛線表示均值
  abline(v = median(dataa[,i]), col = "blue", lwd = 2, lty = 2) # 藍色虛線表示中位數
  legend("topright", legend = c("Mean", "Median"), col = c("red", "blue"), lwd = 2, lty = 2, bty = "n")
}
install.packages("tseries")  # 第一次使用需要安裝
library(tseries)  # 加載 tseries 套件
jarque.bera.test(cex5_small$food)
jarque.bera.test(cex5_small$income)
#b
m1 <- lm(food~income,data=cex5_small)
plot(cex5_small$income,cex5_small$food,pch=16,cex=0.5,xlab = 'INCOME',ylab = 'FOOD')
lines(cex5_small$income, fitted(m1), col = "red", lwd = 2)
(ci_beta2 <- confint(m1, level = 0.95)[2,])
summary(m1)

#c
plot(cex5_small$income, resid(m1), pch = 16, cex = 0.7, 
     xlab = "INCOME", ylab = "Residuals", 
     main = 'Residuals plot')
abline(h = 0, col = "red", lwd = 2)
hist(resid(m1),breaks = 40)
jarque.bera.test(resid(m1))

#d
beta0_hat <- coef(m1)[1]
beta1_hat <- coef(m1)[2]
beta1_CI <- confint(m1,level = 0.95)[2,]

compute_values <- function(income) {
  Y_hat <- beta0_hat + beta1_hat * income  # Fitted FOOD value
  elasticity <- beta1_hat * (income / Y_hat)  # Elasticity
  elasticity_CI <- beta1_CI * (income / Y_hat)  # 95% CI for elasticity
  
  return(data.frame(INCOME = income, 
                    Fitted_FOOD = Y_hat, 
                    Elasticity = elasticity, 
                    Elasticity_Lower = elasticity_CI[1], 
                    Elasticity_Upper = elasticity_CI[2]))
}
income_values <- c(19, 65, 160)  # Income levels to test
(results <- do.call(rbind, lapply(income_values, compute_values)))

#e
cex5_small$lnfood <- log(cex5_small$food)
cex5_small$lnincome <- log(cex5_small$income)
m2 <- lm(lnfood~lnincome,data=cex5_small)
plot(cex5_small$lnincome,cex5_small$lnfood,pch=16,cex=0.7,xlab = 'LNINCOME',ylab = 'LNFOOD')
lines(cex5_small$lnincome, fitted(m2), col = "red", lwd = 2)
summary(m2)

#f
beta0_hat2 <- coef(m2)[1]
beta1_hat2 <- coef(m2)[2]
beta1_CI_loglog <- confint(m2, level = 0.95)[2,]

cat("Point Estimate of Elasticity:", beta1_hat2, "\n")
cat("95% Confidence Interval for Elasticity: (", beta1_CI_loglog[1], ",", beta1_CI_loglog[2], ")\n")

elasticity_loglog <- 0.1863054
se_loglog <- (0.2432675 - 0.1293432) / (2 * 1.96)  # Approximate SE

elasticity_linear <- c(0.07145038, 0.20838756, 0.39319883)  # Elasticities at income 19, 65, 160
se_linear <- c((0.09072601 - 0.05217475) / (2 * 1.96),
               (0.26460562 - 0.15216951) / (2 * 1.96),
               (0.49927462 - 0.28712305) / (2 * 1.96))  # Standard errors


for (i in 1:length(income_values)) {
  z_value <- (elasticity_loglog - elasticity_linear[i]) / sqrt(se_loglog^2 + se_linear[i]^2)
  p_value <- 2 * (1 - pnorm(abs(z_value)))
  
  cat("\nIncome:", income_values[i], "\n")
  cat("Z-value:", z_value, "\n")
  cat("P-value:", p_value, "\n")
  
  if (p_value < 0.05) {
    cat("The two elasticities are significantly different (reject H0).\n")
  } else {
    cat("No significant difference between the two elasticities (fail to reject H0).\n")
  }
}

#g
plot(cex5_small$lnincome, resid(m2), pch = 16, cex = 0.7, 
     xlab = "LNINCOME", ylab = "Residuals", 
     main = 'Residuals plot')
abline(h = 0, col = "red", lwd = 2)
hist(resid(m2),breaks = 40)
jarque.bera.test(resid(m2))

#h
m3 <- lm(food~lnincome,data=cex5_small)
plot(cex5_small$lnincome,cex5_small$food,pch=16,cex=0.7,xlab = 'LNINCOME',ylab = 'FOOD')
lines(cex5_small$lnincome, fitted(m3), col = "red", lwd = 2)
summary(m3)

#i
beta0_hat3 <- coef(m3)[1]
beta1_hat3 <- coef(m3)[2]
beta1_CI3 <- confint(m3,level = 0.95)[2,]

compute_values <- function(income) {
  Y_hat3 <- beta0_hat3 + beta1_hat3 * log(income)  # Fitted FOOD value
  elasticity3 <- beta1_hat3 / income * (income / Y_hat3)  # Elasticity
  elasticity_CI3 <- beta1_CI3 / income * (income / Y_hat3)  # 95% CI for elasticity
  
  # Compute Standard Error (SE) from confidence interval
  SE_elasticity <- (elasticity_CI3[2] - elasticity_CI3[1]) / (2 * 1.96)
  
  return(data.frame(INCOME = income, 
                    Fitted_FOOD = Y_hat3, 
                    Elasticity = elasticity3, 
                    Elasticity_Lower = elasticity_CI3[1], 
                    Elasticity_Upper = elasticity_CI3[2],
                    SE = SE_elasticity))  # Include SE
}

(results <- do.call(rbind, lapply(income_values, compute_values)))

num_incomes <- length(income_values)
for (i in 1:(num_incomes - 1)) {
  for (j in (i + 1):num_incomes) {
    
    # Extract elasticities and SE for both income levels
    E1 <- results$Elasticity[i]
    SE1 <- results$SE[i]
    
    E2 <- results$Elasticity[j]
    SE2 <- results$SE[j]
    
    # Compute Z-score
    z_value <- (E1 - E2) / sqrt(SE1^2 + SE2^2)
    
    # Compute p-value (two-tailed test)
    p_value <- 2 * (1 - pnorm(abs(z_value)))
    
    # Print results
    cat("\nComparing Elasticities: INCOME =", income_values[i], "vs", income_values[j], "\n")
    cat("P-value:", p_value, "\n")
    if (p_value < 0.05) {
      cat("The two elasticities are significantly different (reject H0).\n")
    } else {
      cat("No significant difference between the two elasticities (fail to reject H0).\n")
    }
  }
}


#j
plot(cex5_small$lnincome, resid(m3), pch = 16, cex = 0.7, 
     xlab = "LNINCOME", ylab = "Residuals", 
     main = 'Residuals plot')
abline(h = 0, col = "red", lwd = 2)
hist(resid(m3),breaks = 40)
jarque.bera.test(resid(m3))
