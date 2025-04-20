url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/vacation.rdata"
file_path <- "vacation.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(vacation)

##8.16.1
model_ols <- lm(miles ~ income + age + kids, data = vacation)
summary(model_ols)
confint(model_ols, level = 0.95)["kids", ]

##8.16.2
vacation$residuals <- resid(model_ols)

plot(vacation$income, vacation$residuals,
     xlab = "Income",
     ylab = "OLS Residuals",
     main = "Residuals vs Income",
     pch = 19, col = "steelblue")
abline(h = 0, lty = 2, col = "red")

plot(vacation$age, vacation$residuals,
     xlab = "Age",
     ylab = "OLS Residuals",
     main = "Residuals vs Age",
     pch = 19, col = "steelblue")
abline(h = 0, lty = 2, col = "red")

##8.16.3
vacation_sorted <- vacation[order(vacation$income), ]
vacation_low  <- vacation_sorted[1:90, ]
vacation_high <- vacation_sorted[111:200, ]  
model_low  <- lm(miles ~ income + age + kids, data = vacation_low)
model_high <- lm(miles ~ income + age + kids, data = vacation_high)
SSR_low  <- sum(resid(model_low)^2)
SSR_high <- sum(resid(model_high)^2)
SSR_low
SSR_high
df_low  <- nrow(vacation_low)  - length(coef(model_low))  # 90 - 4 = 86
df_high <- nrow(vacation_high) - length(coef(model_high)) # 90 - 4 = 86
F_stat <- (SSR_high / df_high) / (SSR_low / df_low)
F_stat

##8.16.4
install.packages("sandwich")
install.packages("lmtest")
library(sandwich)
library(lmtest)
robust_se <- vcovHC(model_ols, type = "HC1")
coeftest(model_ols, vcov. = robust_se)
coefci(model_ols, vcov. = robust_se, level = 0.95)["kids", ]

##8.16.5
vacation_gls <- vacation[vacation$income > 0, ]
model_gls <- lm(miles ~ income + age + kids,
                data = vacation_gls,
                weights = 1 / (income^2))
summary(model_gls)

robust_se_gls <- vcovHC(model_gls, type = "HC1")
coeftest(model_gls, vcov. = robust_se_gls)
coefci(model_gls, vcov. = robust_se_gls)["kids", ]
confint(model_gls)["kids", ]



