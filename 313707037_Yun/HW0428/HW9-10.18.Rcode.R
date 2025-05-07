url <- "https://www.principlesofeconometrics.com/poe5/data/ascii/mroz.dat"
download.file(url, destfile = "mroz.dat")
data <- read.table("mroz.dat", header = FALSE)
colnames(data) <- c("TAXABLEINC","FEDERALTAX","HSIBLINGS","HFATHEREDUC","HMOTHEREDUC","SIBLINGS","LFP","HOURS","KIDSL6","KIDS618","AGE","EDUC","WAGE","WAGE76","HHOURS","HAGE","HEDUC","HWAGE","FAMINC","MTR","MOTHEREDUC","FATHEREDUC","UNEMPLOYMENT","LARGECITY","EXPER"
)
head(data)  # 查看前幾行

#(a)
data$MOTHERCOLL <- ifelse(data$MOTHEREDUC > 12, 1, 0)
data$FATHERCOLL <- ifelse(data$FATHEREDUC > 12, 1, 0)

# 計算有大學教育的父母百分比
mean(data$MOTHERCOLL, na.rm = TRUE)
mean(data$FATHERCOLL, na.rm = TRUE)

#(b)
cor(data[, c("EDUC", "MOTHERCOLL", "FATHERCOLL")], use = "complete.obs")


#(d)
first_stage_d <- lm(EDUC ~ MOTHERCOLL, data = data)
first_stage_d <- lm(EDUC ~ MOTHERCOLL + EXPER + EXPER2, data = data)
summary(first_stage_d)
f_test <- summary(first_stage_d)$fstatistic
cat("F-statistic for MOTHERCOLL:", f_test[1], "\n")

#(e)
iv_model_e <- ivreg(log(WAGE) ~ EXPER + EXPER2 + EDUC | MOTHERCOLL + FATHERCOLL+EXPER + EXPER2, data = data)
summary(iv_model_e, diagnostics = TRUE)
confint(iv_model_e, "EDUC", level = 0.95)

#(c)
summary(data$WAGE)
table(data$WAGE <= 0)  # 檢查多少個 WAGE 小於等於 0
table(data$WAGE > 0)
data_clean <- subset(data, WAGE > 0)
data_clean$EXPER2 <- data_clean$EXPER^2

library(AER)
iv_model <- ivreg(log(WAGE) ~ EDUC + EXPER + EXPER2 | MOTHERCOLL + EXPER + EXPER2, data = data_clean)
summary(iv_model)
confint(iv_model, "EDUC", level = 0.95)

#(e)
iv_model2 <- ivreg(log(WAGE) ~ EDUC + EXPER + EXPER2 | MOTHERCOLL + FATHERCOLL + EXPER + EXPER2, data = data_clean)
summary(iv_model2)
confint(iv_model2, "EDUC", level = 0.95)

#(f)
first_stage_f <- lm(EDUC ~ MOTHERCOLL + FATHERCOLL, data = data)
summary(first_stage_f)
anova(first_stage_f)  # 可用於聯合檢定

#(g)
library(sandwich)
library(lmtest)
# Hansen's J-test for overidentification
summary(iv_model2, diagnostics = TRUE)$diagnostics