rm(list = ls())  
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/wa_wheat.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
wa_wheat

#4.28 a
northampton <- wa_wheat$northampton
time <- wa_wheat$time
#draw model1
model1 <- lm(northampton~time, wa_wheat)
ggplot(wa_wheat, aes(x = time, y = northampton))+
  geom_point(color = "black", alpha = 0.5)+
  geom_line(data = wa_wheat, aes(x = time, y = northampton), color = "grey", size = 0.5) +
  labs(
    title = "Fitted Value for Model 1",
    x = "Time(Years)",
    y = "Average Wheat Yield in Tonnes per Hectare"
  )+
  theme_minimal()
#draw residuals for model1
predicted_wheat1 <- fitted(model1)  
residuals1 <- northampton - predicted_wheat1
ggplot(wa_wheat, aes(x = time, y = residuals1))+
  geom_point(color = "black", alpha = 0.5)+
  labs(
    title = "Residuals from Model 1",
    x = "Time(Years)",
    y = "Residuals"
  )+
  theme_minimal()
#error normality test
shapiro_test <- shapiro.test(resid(model1))
print(shapiro_test)
model1_summary <- summary(model1)
#R square
r_squared1 <- model1_summary$r.squared
cat("R-squared for Model 1:", r_squared1)

#draw model2
model2 <- lm(northampton~log(time), wa_wheat)
ggplot(wa_wheat, aes(x = log(time), y = northampton))+
  geom_point(color = "black", alpha = 0.5)+
  geom_line(data = wa_wheat, aes(x = log(time), y = northampton), color = "grey", size = 0.5) +
  labs(
    title = "Fitted Value for Model 2",
    x = "Log(Time) (Years)",
    y = "Average Wheat Yield in Tonnes per Hectare"
  )+
  theme_minimal()
#draw residuals for model2
predicted_wheat2 <- fitted(model2)  
residuals2 <- northampton - predicted_wheat2
ggplot(wa_wheat, aes(x = log(time), y = residuals2))+
  geom_point(color = "black", alpha = 0.5)+
  labs(
    title = "Residuals from Model 2",
    x = "log(Time) (Years)",
    y = "Residuals"
  )+
  theme_minimal()
#error normality test
shapiro_test <- shapiro.test(resid(model2))
print(shapiro_test)
model2_summary <- summary(model2)
#R square
r_squared2 <- model2_summary$r.squared
cat("R-squared for Model 2:", r_squared2)

#draw model3
model3 <- lm(northampton~time^2, wa_wheat)
ggplot(wa_wheat, aes(x = time^2, y = northampton))+
  geom_point(color = "black", alpha = 0.5)+
  geom_line(data = wa_wheat, aes(x = time^2, y = northampton), color = "grey", size = 0.5) +
  labs(
    title = "Fitted Value for Model 3",
    x = "Time^2 (Years)",
    y = "Average Wheat Yield in Tonnes per Hectare"
  )+
  theme_minimal()
#draw residuals for model3
predicted_wheat3 <- fitted(model3)  
residuals3 <- northampton - predicted_wheat3
ggplot(wa_wheat, aes(x = time^2, y = residuals3))+
  geom_point(color = "black", alpha = 0.5)+
  labs(
    title = "Residuals from Model 3",
    x = "Time^2 (Years)",
    y = "Residuals"
  )+
  theme_minimal()
#error normality test
shapiro_test <- shapiro.test(resid(model3))
print(shapiro_test)
model3_summary <- summary(model3)
#R square
r_squared3 <- model3_summary$r.squared
cat("R-squared for Model 3:", r_squared3)

#draw model4
model4 <- lm(log(northampton)~time, wa_wheat)
ggplot(wa_wheat, aes(x = time, y =log(northampton) ))+
  geom_point(color = "black", alpha = 0.5)+
  geom_line(data = wa_wheat, aes(x = time, y = log(northampton) ), color = "grey", size = 0.5) +
  labs(
    title = "Fitted Value for Model 4",
    x = "Time(Years)",
    y = "Log Average Wheat Yield in Tonnes per Hectare"
  )+
  theme_minimal()
#draw residuals for model4
predicted_wheat4 <- fitted(model4)  
residuals4 <- northampton - predicted_wheat4
ggplot(wa_wheat, aes(x = time, y = residuals4))+
  geom_point(color = "black", alpha = 0.5)+
  labs(
    title = "Residuals from Model 4",
    x = "Time(Years)",
    y = "Residuals"
  )+
  theme_minimal()
#error normality test
shapiro_test <- shapiro.test(resid(model4))
print(shapiro_test)
model4_summary <- summary(model4)
#R square
r_squared4 <- model4_summary$r.squared
cat("R-squared for Model 4:", r_squared4) 

#better value
sse1 <- sum(residuals1^2)
sse2 <- sum(residuals2^2)
sse3 <- sum(residuals3^2)
sse4 <- sum(residuals4^2)
col <- tibble(sse1, sse2, sse3, sse4)
col
  

#4.28 c
summary(model3)

stud_resid <- rstudent(model3)
leverage_vals <- hatvalues(model3)
dfbetas_vals <- dfbetas(model3)
dffits_vals <- dffits(model3)
threshold_resid <- 2
unusual_resid <- which(abs(stud_resid) > threshold_resid)

# For leverage: a common rule is that observations with leverage > 2*(p/n) are high-leverage,
# where p is the number of predictors including the intercept and n is the sample size.
p <- length(coef(model3))  # number of coefficients
n <- nrow(model.frame(model3))
threshold_leverage <- 2 * p / n
unusual_leverage <- which(leverage_vals > threshold_leverage)

# For DFBETAS: a rule of thumb is that an absolute DFBETAS > 2/sqrt(n) for any coefficient indicates influence.
threshold_dfbetas <- 2 / sqrt(n)
# Identify observations that exceed the threshold for any predictor
unusual_dfbetas <- which(apply(abs(dfbetas_vals), 1, max) > threshold_dfbetas)

# For DFFITS: a common threshold is |DFFITS| > 2*sqrt(p/n).
threshold_dffits <- 2 * sqrt(p / n)
unusual_dffits <- which(abs(dffits_vals) > threshold_dffits)

# -------------------------------
# Print or Tabulate the Results
# -------------------------------

cat("Unusual observations based on Studentized Residuals (>|", threshold_resid, "|):\n")
print(unusual_resid)

cat("\nUnusual observations based on Leverage (>", round(threshold_leverage, 3), "):\n")
print(unusual_leverage)

cat("\nUnusual observations based on DFBETAS (>|", round(threshold_dfbetas, 3), "| for any coefficient):\n")
print(unusual_dfbetas)

cat("\nUnusual observations based on DFFITS (>|", round(threshold_dffits, 3), "|):\n")
print(unusual_dffits)


#4.28 d

data_train <- subset.data.frame(wa_wheat, select = time < 48,)
# Fit your chosen model; here we use a log-linear model as an example.
model5 <- lm(northampton ~ time^2, data = data_train)
summary(model5)
# Create new data for prediction for 1997, assuming time = 48 corresponds to 1997
new_data <- data.frame(time = 48)
# Construct a 95% prediction interval
pred <- predict(model5, newdata = new_data, interval = "prediction", level = 0.95)
# If the model is in log-scale, transform predictions back:
pred_exp <- exp(pred)
print(pred_exp)
# Get the true yield for 1997 (time = 48)
true_value <- wa_wheat$northampton[wa_wheat$time == 48]
cat("True yield for 1997:", true_value, "\n")
cat("95% prediction interval for 1997 (exponentiated): [", pred_exp[,"lwr"], ",", pred_exp[,"upr"], "]\n")

if (true_value >= pred_exp[,"lwr"] && true_value <= pred_exp[,"upr"]) {
  cat("The true value is contained in the prediction interval.\n")
} else {
  cat("The true value is NOT contained in the prediction interval.\n")
}


