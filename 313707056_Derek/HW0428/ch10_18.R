library(POE5Rdata)
library(car)
library(AER)        # ivreg
data("mroz")

mroz$MOTHERCOLL <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$FATHERCOLL <- ifelse(mroz$fathereduc > 12, 1, 0)
mean(mroz$MOTHERCOLL, na.rm = TRUE)  
mean(mroz$FATHERCOLL, na.rm = TRUE) 

cor(mroz$educ,mroz$MOTHERCOLL)
cor(mroz$educ,mroz$FATHERCOLL)
cor(mroz$MOTHERCOLL,mroz$FATHERCOLL)
# 計算相關係數矩陣
cor(mroz[, c("educ", "MOTHERCOLL", "FATHERCOLL")], use = "complete.obs")

sum(mroz$wage == 0, na.rm = TRUE)
mroz_clean <- subset(mroz, wage > 0 & !is.na(wage) & !is.na(exper) & !is.na(educ))
marz_ols<-lm(log(wage)~exper+I(exper^2)+educ, data=mroz_clean)
summary(marz_ols)

# IV regression: wage ~ educ (MOTHERCOLL 作為 IV)
iv1 <- ivreg(log(wage)~exper+I(exper^2)+educ|exper+I(exper^2)+MOTHERCOLL, data = mroz_clean)
summary(iv1)



# 95% 信賴區間
confint(iv1)

#d
educ.ols <- lm(educ~MOTHERCOLL,data=mroz_clean)
summary(educ.ols)
linearHypothesis(educ.ols,c("MOTHERCOLL=0"))
#e
iv2 <- ivreg(log(wage)~exper+I(exper^2)+educ | exper+I(exper^2)+MOTHERCOLL+FATHERCOLL, data = mroz_clean)
summary(iv2)
confint(iv2)

#f
educ.ols2 <- lm(educ~MOTHERCOLL+FATHERCOLL,data=mroz_clean)
summary(educ.ols2)
#g
summary(iv2,diagnostics = TRUE)
