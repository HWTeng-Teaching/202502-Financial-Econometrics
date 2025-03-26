#loading up the data
library(PoEdata)
data(capm4)

head(capm4)

#calculating the excess returns
capm4$dis_excess <- capm4$dis - capm4$riskfree
capm4$ge_excess <- capm4$ge - capm4$riskfree
capm4$gm_excess <- capm4$gm - capm4$riskfree
capm4$ibm_excess <- capm4$ibm - capm4$riskfree
capm4$msft_excess <- capm4$msft - capm4$riskfree
capm4$xom_excess <- capm4$xom - capm4$riskfree
capm4$mkt_excess <- capm4$mkt - capm4$riskfree

#regresssion models for each stock
dis_model <- lm(dis_excess ~ mkt_excess, data = capm4)
ibm_model <- lm(ibm_excess ~ mkt_excess, data = capm4)
ge_model <- lm(ge_excess ~ mkt_excess, data = capm4)
gm_model <- lm(gm_excess ~ mkt_excess, data = capm4)
msft_model <- lm(msft_excess ~ mkt_excess, data = capm4)
xom_model <- lm(xom_excess ~ mkt_excess, data = capm4)

#summary of the regression models
summary(ge_model)
summary(dis_model)
summary(gm_model)
summary(ibm_model)
summary(msft_model)
summary(xom_model)

#plotting the microsoft regression model
plot(capm4$mkt_excess, capm4$msft_excess,
    xlab = "Market Excess Return (MKT - RISKFREE)",
    ylab = "Microsoft Excess Return (MSFT - RISKFREE)",
    main = "CAPM: Microsoft Excess Return vs Market Excess Return",
    pch = 19, col = "blue"
)

#adding the regression line
abline(msft_model, col = "red")

#forcing the intercept to be 0
msft_model0 <- lm(msft_excess ~ mkt_excess - 1, data = capm4)
dis_model0 <- lm(dis_excess ~ mkt_excess, -1, data = capm4)
ibm_model0 <- lm(ibm_excess ~ mkt_excess, -1, data = capm4)
ge_model0 <- lm(ge_excess ~ mkt_excess, -1, data = capm4)
gm_model0 <- lm(gm_excess ~ mkt_excess, -1, data = capm4)
xom_model0 <- lm(xom_excess ~ mkt_excess, -1, data = capm4)
summary(msft_model0)
summary(dis_model0)
summary(ibm_model0)
summary(ge_model0)
summary(gm_model0)
summary(xom_model0)
