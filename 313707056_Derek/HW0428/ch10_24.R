library(POE5Rdata)
library(car)
library(AER)  # ivreg
library(ggplot2)
library(broom)
library(lmtest)
library(sandwich)
library(boot)
data("mroz")


mroz$MOTHERCOLL <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$FATHERCOLL <- ifelse(mroz$fathereduc > 12, 1, 0)

sum(mroz$wage == 0, na.rm = TRUE)
mroz_clean <- subset(mroz, wage > 0 & !is.na(wage) & !is.na(exper) & !is.na(educ))
marz_ols<-lm(log(wage)~exper+I(exper^2)+educ, data=mroz_clean)
summary(marz_ols)

iv2 <- ivreg(log(wage)~exper+I(exper^2)+educ | exper+I(exper^2)+MOTHERCOLL+FATHERCOLL, data = mroz_clean)
summary(iv2)

mroz_clean$IV_2SLS_residuals <- iv2$residuals

ggplot(data = mroz_clean,aes(x=exper))+
  geom_point(aes(y=IV_2SLS_residuals))

ggplot(data = mroz_clean,aes(x=exper,y=I(IV_2SLS_residuals^2)))+
  geom_point()

ressq <- iv2$residuals^2
modres <- lm(ressq~mroz_clean$exper)
bptest(modres)
#c
summary(iv2,robust = TRUE)
coeftest(iv2, vcov = vcovHC(iv2))
confint(iv2, level = 0.95)

#d
iv_bootstrap <- function(data, indices) {
  # 使用給定的重抽樣索引（indices）來擷取樣本
  d <- data[indices, ]
  
  # 使用ivreg進行IV/2SLS估計
  model <- ivreg(log(wage)~exper+I(exper^2)+educ | exper+I(exper^2)+MOTHERCOLL+FATHERCOLL, data = mroz_clean)
  
  # 返回EDUC係數
  return(coef(model)["educ"])
}

# 執行引導抽樣，使用200次重抽樣
bootstrap_results <- boot(data = mroz_clean, statistic = iv_bootstrap, R = 200)

# 顯示引導的95%區間
boot.ci(bootstrap_results, type = "bca")

50*pnorm(0.2252)-51*exp(-0.06*9/12)*pnorm(-0.0173)
19*exp(-0.06)*pnorm(-0.3202)-20*pnorm(-0.5702)
qnorm(0.5702)
