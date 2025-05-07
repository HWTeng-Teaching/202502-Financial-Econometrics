library(AER)
library(POE5Rdata)

data('mroz')

# (a)
mroz_lfp <- subset(mroz, lfp == 1)
mroz_lfp$mothercoll <- ifelse(mroz_lfp$mothereduc > 12, 1, 0)
mroz_lfp$fathercoll <- ifelse(mroz_lfp$fathereduc > 12, 1, 0)
mroz_lfp$parentcoll <- ifelse(mroz_lfp$mothereduc > 12 | mroz_lfp$fathereduc > 12 , 1, 0)
mean(mroz_lfp$parentcoll)

# (b)
subset_data <- mroz_lfp[, c("educ", "mothercoll", "fathercoll")]
cor_matrix <- cor(subset_data)
cor_matrix

# (c)
iv_model <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                    exper + I(exper^2) + mothercoll,
                  data = subset(mroz_lfp, wage > 0))
summary(iv_model)
confint(iv_model, level = 0.95)["educ", ]

# (d)
first_stage <- lm(educ ~ exper + I(exper^2) + mothercoll, data = mroz_lfp)
summary(first_stage)
anova(first_stage)

# (e)
iv_model_2 <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                      exper + I(exper^2) + mothercoll + fathercoll,
                    data = subset(mroz_lfp, wage > 0))
summary(iv_model_2)
confint(iv_model_2, level = 0.95)["educ", ]

# (f)
first_stage_2 <- lm(educ ~ exper + I(exper^2) + mothercoll + fathercoll, data = mroz_lfp)
summary(first_stage_2)
linearHypothesis(first_stage_2, c("mothercoll = 0", "fathercoll = 0"))

# (g)
resid_iv <- resid(iv_model_2)
sargan_test <- lm(resid_iv ~ mothercoll + fathercoll, data = mroz_lfp)
summary(sargan_test)
S <- nrow(mroz_lfp) * summary(sargan_test)$r.squared
(p_value <- 1 - pchisq(S, df = 1))
