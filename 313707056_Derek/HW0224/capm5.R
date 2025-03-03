#devtools::install_github("ccolonescu/POE5Rdata")
library(POE5Rdata)
data("capm5")
modGE<-lm(capm5$ge-capm5$riskfree~capm5$mkt-capm5$riskfree,data=capm5)
summary(modGE)

modIBM<-lm(capm5$ibm-capm5$riskfree~capm5$mkt-capm5$riskfree,data=capm5)
summary(modIBM)

modFord<-lm(capm5$ford-capm5$riskfree~capm5$mkt-capm5$riskfree,data=capm5)
summary(modFord)

modmsft<-lm(capm5$msft-capm5$riskfree~capm5$mkt-capm5$riskfree,data=capm5)
summary(modmsft)
plot(capm5$mkt-capm5$riskfree,capm5$msft-capm5$riskfree,
     xlab = "rm-rf",
     ylab = "rmsft-rf")
abline(modmsft$coefficients[1],modmsft$coefficients[2])

moddis<-lm(capm5$dis-capm5$riskfree~capm5$mkt-capm5$riskfree,data=capm5)
summary(moddis)

modxom<-lm(capm5$xom-capm5$riskfree~capm5$mkt-capm5$riskfree,data=capm5)
summary(modxom)



modGE2<-lm(capm5$ge-capm5$riskfree~capm5$mkt-capm5$riskfree-1,data=capm5)
summary(modGE2)

modIBM2<-lm(capm5$ibm-capm5$riskfree~capm5$mkt-capm5$riskfree-1,data=capm5)
summary(modIBM2)

modFord2<-lm(capm5$ford-capm5$riskfree~capm5$mkt-capm5$riskfree-1,data=capm5)
summary(modFord2)

modmsft2<-lm(capm5$msft-capm5$riskfree~capm5$mkt-capm5$riskfree-1,data=capm5)
summary(modmsft2)

moddis2<-lm(capm5$dis-capm5$riskfree~capm5$mkt-capm5$riskfree-1,data=capm5)
summary(moddis2)

modxom2<-lm(capm5$xom-capm5$riskfree~capm5$mkt-capm5$riskfree-1,data=capm5)
summary(modxom2)