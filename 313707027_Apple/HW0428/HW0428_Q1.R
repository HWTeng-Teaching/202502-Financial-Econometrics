# 10.18
library(POE5Rdata)
data(mroz)

# (a)
library(dplyr)
mroz_lfp <- subset(mroz, lfp == 1)
working_wives <- mroz_lfp %>%
  filter(!is.na(wage)) %>% 
  mutate(
    mothercoll = ifelse(mothereduc > 12, 1, 0),
    fathercoll = ifelse(fathereduc > 12, 1, 0),
    parentscoll = ifelse(mothereduc > 12 | fathereduc > 12 , 1, 0)
  )
mean(working_wives$mothercoll)  
mean(working_wives$fathercoll)
mean(working_wives$parentscoll)

# (b)
cor(working_wives[, c("educ", "mothercoll", "fathercoll")], use = "complete.obs")

# (c)
library(AER)
mroz_lf <- subset(mroz, lfp == 1 & !is.na(wage) & wage > 0)
mroz_lf$MOTHERCOLL <- as.numeric(mroz_lf$mothereduc > 12)
mroz_lf$FATHERCOLL <- as.numeric(mroz_lf$fathereduc > 12)
iv_model1 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + exper + I(exper^2), data = mroz_lf)
summary(iv_model1)
confint(iv_model1, level = 0.95)

# (d)
first_stage <- lm(educ ~ MOTHERCOLL + exper + I(exper^2), data = mroz_lf)
summary(first_stage)
library(car)
linearHypothesis(first_stage, "MOTHERCOLL = 0")

# (e)
iv_model2 <- ivreg(log(wage) ~ educ + exper + I(exper^2) |
                     MOTHERCOLL + FATHERCOLL + exper + I(exper^2),
                   data = mroz_lf)

summary(iv_model2)
confint(iv_model2, "educ", level = 0.95)

# (f)
first_stage2 <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz_lf)
summary(first_stage2)
linearHypothesis(first_stage2, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

# (g)
summary(iv_model2, diagnostics = TRUE)