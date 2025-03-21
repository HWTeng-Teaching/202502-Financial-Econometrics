# 4.28
# c

model_diagnostics <- influence.measures(model3)

stud_resid <- rstudent(model3)

leverage <- hatvalues(model3)

dfbetas_values <- dfbetas(model3)

dffits_values <- dffits(model3)

threshold_stud_resid <- 2  #
threshold_leverage <- 2 * mean(leverage)  
threshold_dffits <- 2 * sqrt(ncol(model.matrix(model3)) / nrow(wa_wheat))  

outliers_stud_resid <- which(abs(stud_resid) > threshold_stud_resid)
outliers_leverage <- which(leverage > threshold_leverage)
outliers_dffits <- which(abs(dffits_values) > threshold_dffits)


list(
  "Outliers (Studentized Residuals)" = outliers_stud_resid,
  "High Leverage Points" = outliers_leverage,
  "Influential Points (DFFITS)" = outliers_dffits
)


# d

train_data <- subset(wa_wheat, time <= 47)

model3_train <- lm(northampton ~ I(time^2), data = train_data)

new_data_1997 <- data.frame(time = 1997)
pred_1997 <- predict(model3_train, newdata = new_data_1997, interval = "prediction", level = 0.95)

pred_1997

 #      fit      lwr      upr
 # 1899.421 1485.655 2313.187

# Yes, the prediction interval contains the true value.