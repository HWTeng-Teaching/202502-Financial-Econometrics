url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"
file_path <- "mroz.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
nrow(mroz)

##10.18.1
mroz_lfp <- subset(mroz, lfp == 1)
nrow(mroz_lfp)
mroz_lfp$MOTHERCOLL <- as.numeric(mroz_lfp$mothereduc > 12)
mroz_lfp$FATHERCOLL <- as.numeric(mroz_lfp$fathereduc > 12)
mean(mroz_lfp$MOTHERCOLL) * 100 
mean(mroz_lfp$FATHERCOLL) * 100 

##10.18.2
cor(mroz_lfp[, c("educ", "MOTHERCOLL", "FATHERCOLL")], use = "complete.obs")

##10.18.3
install.packages("AER")
library(AER)
iv1 <- ivreg(
  log(wage) ~ educ + exper + I(exper^2) |    
    MOTHERCOLL + exper + I(exper^2),     
  data = mroz_lfp
)
summary(iv1, diagnostics = TRUE)  
confint(iv1, level = 0.95)["educ", ]

##10.18.4
first_stage_c <- lm(educ ~ MOTHERCOLL+ exper + I(exper^2), data = mroz_lfp)
summary(first_stage_c)  
linearHypothesis(first_stage_c, c("MOTHERCOLL = 0"))

##10.18.5
iv_model_e <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz_lfp)
summary(iv_model_e)
confint(iv_model_e, level = 0.95)["educ", ]

##10.18.6
first_stage_f <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz_lfp)
summary(first_stage_f)
library(car)
linearHypothesis(first_stage_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

##10.18.7
summary(iv_model_e, diagnostics = TRUE)





