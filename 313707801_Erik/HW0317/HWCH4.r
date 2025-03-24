#load the data, get a an overview of it
library(PoEdata)
data(wa_wheat)
head(wa_wheat)

#clean up the data a bit
df$TIME <- df$time
df$YIELD <- df$northampton

#Estimate the four models
model1 <- lm(YIELD ~ TIME, data = df)
model2 <- lm(YIELD ~ log(TIME), data = df)
model3 <- lm(YIELD ~ I(TIME^2), data = df)
model4 <- lm(log(YIELD) ~ TIME, data = df)

#Add the fitted values to the data frame
df$fitted1 <- fitted(model1)
df$fitted2 <- fitted(model2)
df$fitted3 <- fitted(model3)
df$fitted4 <- exp(fitted(model4))


#plotting the four models
ggplot(df, aes(x = TIME)) +
    geom_point(aes(y = YIELD), color = "black", size = 2) +
    geom_line(aes(y = fitted1), color = "blue", linetype = "solid") +
    geom_line(aes(y = fitted2), color = "green", linetype = "dashed") +
    geom_line(aes(y = fitted3), color = "red", linetype = "dotdash") +
    geom_line(aes(y = fitted4), color = "purple", linetype = "twodash") +
    labs(
        title = "Wheat Yield in Northampton: Model Fits",
        y = "Yield", x = "Time (Years since 1950)"
    ) +
    theme_minimal()

#plotting the residuals
par(mfrow = c(2, 2))
plot(resid(model1), main = "Residuals: Linear")
plot(resid(model2), main = "Residuals: Log(TIME)")
plot(resid(model3), main = "Residuals: TIME²")
plot(resid(model4), main = "Residuals: log(YIELD)")
par(mfrow = c(1, 1))

#testing the residuals for normality
library(nortest)

shapiro.test(resid(model1))
shapiro.test(resid(model2))
shapiro.test(resid(model3))
shapiro.test(resid(model4))

#finding the R-squared values
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared
summary(model4)$r.squared