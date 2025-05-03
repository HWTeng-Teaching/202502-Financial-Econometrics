#10.18
#a
data(mroz)
mroz_lfp <- subset(mroz, lfp == 1)
mroz_lfp$mothercoll <- ifelse(mroz_lfp$mothereduc > 12, 1, 0)
mroz_lfp$fathercoll <- ifelse(mroz_lfp$fathereduc > 12, 1, 0)
mroz_lfp$parentcoll <- ifelse(mroz_lfp$mothereduc > 12 | mroz_lfp$fathereduc > 12 , 1, 0)
mean(mroz_lfp$parentcoll)
cat('Conclusion:\n18.7% have at least one parent with more than 12 years of education')

#b
cor(mroz_lfp[, c("educ", "mothercoll", "fathercoll")])
cat('Conclusion:\nBoth MOTHERCOLL and FATHERCOLL show moderate positive correlations with the woman’s years of education. These magnitudes suggest potentially relevant instruments.\nUsing indicators like MOTHERCOLL and FATHERCOLL may better isolate exogenous variation in education, reducing concerns about fine-grained endogeneity from continuous parental education variables.')

#c
mroz_lfp$lwage <- log(mroz_lfp$wage)
mroz_lfp$exper2 <- mroz_lfp$exper^2
#install.packages("AER")
library(AER)
iv_model <- ivreg(lwage ~ exper + exper2 + educ | exper + exper2 + mothercoll, data = mroz_lfp)
summary(iv_model, diagnostics = TRUE)
confint(iv_model, level = 0.95)["educ", ]
cat('95% Confidence Interval: [',confint(iv_model, level = 0.95)["educ", ],']')

#d
first_stage <- lm(educ ~ exper + I(exper^2) + mothercoll, data = mroz_lfp)
summary(first_stage)
library(car)
Anova(first_stage, type = "II")
cat('Conclusion:\nMOTHERCOLL is a strong instrument (F=',Anova(first_stage, type = "II")["mothercoll", "F value"] ,'> 10), meaning it has high predictive power for EDUC')

#e
iv_model_2inst <- ivreg(lwage ~ exper + I(exper^2) + educ |
                          exper + I(exper^2) +mothercoll + fathercoll,
                        data = mroz_lfp)

summary(iv_model_2inst, diagnostics = TRUE)
confint(iv_model_2inst, level = 0.95)["educ", ]
cat('95% Confidence Interval: [',confint(iv_model_2inst, level = 0.95)["educ", ],']','\nConclusion:\nThe estimate is higher and statistically significant at the 1% level.The confidence interval is narrower than in (c), showing increased precision when both instruments are used.')

#f
first_stage_2inst <- lm(educ ~ exper + I(exper^2) + mothercoll + fathercoll, data = mroz_lfp)
summary(first_stage_2inst)
Anova(first_stage_2inst, type = "II")
cat('F value of MOTHERCOLL is', Anova(first_stage_2inst, type = "II")["mothercoll", "F value"],', p-value is',Anova(first_stage_2inst, type = "II")["mothercoll", "Pr(>F)"],'<0.001.\nF value of FATHERCOLL is', Anova(first_stage_2inst, type = "II")["fathercoll", "F value"],', p-value is',Anova(first_stage_2inst, type = "II")["fathercoll", "Pr(>F)"],'<0.001.',
'\nConclusion:\nBased on the Type II ANOVA table, both MOTHERCOLL and FATHERCOLL are jointly significant predictors of EDUC.Their F values and extremely small p-values indicate that these instruments are strong and jointly relevant, satisfying the relevance condition for valid IV estimation.')

#g
resid_iv <- resid(iv_model_2inst)
sargan_test <- lm(resid_iv ~ mothercoll + fathercoll, data = mroz_lfp)
S <- nrow(mroz_lfp) * summary(sargan_test)$r.squared
(p_value <- 1 - pchisq(S, df = 1))
cat('Test statistic S=n*R_squared=',S,', p-value = ',p_value,'\nConclusion:\nWe do not reject the null hypothesis that the instruments are valid. There is no evidence of overidentifying restriction violations. MOTHERCOLL and FATHERCOLL are exogenous and valid instruments.')

#10.20
#a
data(capm5)
capm5$msft_excess <- capm5$msft - capm5$riskfree
capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm_model <- lm(msft_excess ~ mkt_excess, data = capm5)
summary(capm_model)
cat('beta=',summary(capm_model)$coef[2],"Conclusion:\nMicrosoft exhibits a beta greater than 1, indicating that it is more volatile than the market and thus a risky stock during this period.")

#b
capm5$rank <- rank(capm5$mkt_excess, ties.method = "first")
first_stage <- lm(mkt_excess ~ rank, data = capm5)
summary(first_stage)
cat('RANK p-value <2e-16,indicating the coefficient is very significant.\n','R squared =', summary(first_stage)$r.squared,'\nConclusion:\nRANK strongly predicts market excess return and is a strong instrument.')

#c
capm5$v_hat <- resid(first_stage)
augmented_model <- lm(msft_excess ~ mkt_excess + v_hat, data = capm5)
summary(augmented_model)
cat('v_hat p-value = 0.0428\n',"Conclusion:\nAt the 1% significance level, we cannot reject the null hypothesis that the market return is exogenous. Thus, OLS is consistent.")

#d
iv_model <- ivreg(msft_excess ~ mkt_excess | rank, data = capm5)
summary(iv_model)
cat('Using RANK as the instrument: beta =', summary(iv_model)$coef[2], 'higher than OLS.\n',
    'Conclusion:\nThe IV estimate corrects the likely attenuation bias from measurement error, consistent with theoretical expectations.')
#e
capm5$pos <- ifelse(capm5$mkt_excess > 0, 1, 0)
first_stage_multiIV <- lm(mkt_excess ~ rank + pos, data = capm5)
summary(first_stage_multiIV)
anova(first_stage_multiIV)

cat('First-stage regression with RANK and POS: R squared=',summary(first_stage_multiIV)$r.squared, '\nJoint F-test shows both variables are significant at 5% level (Rank p-value<2e-16 and POS p-value=0.0291.)\n',
    'Conclusion:\nRANK and POS together form a strong set of instruments.')

#f
capm5$v_hat2 <- resid(first_stage_multiIV)
augmented_model2 <- lm(msft_excess ~ mkt_excess + v_hat2, data = capm5)
summary(augmented_model2)
cat('p-value of residuals =',summary(augmented_model2)$coef["v_hat2", "Pr(>|t|)"]
    ,'\nConclusion: Still not significant at 1%, so we do not reject exogeneity of market return.')

#g
iv_model2 <- ivreg(msft_excess ~ mkt_excess | rank + pos, data = capm5)
summary(iv_model2)
cat('Estimated beta =', summary(iv_model2)$coef[2],', higher than OLS beta=',summary(capm_model)$coef[2],
    '\nConclusion:\nThe result confirms measurement error bias in OLS. The IV estimate is more robust and aligns with economic theory.')

#h
capm5$resid_iv2 <- resid(iv_model2)
sargan_reg <- lm(resid_iv2 ~ rank+ pos, data = capm5)
summary(sargan_reg)
R2 <- summary(sargan_reg)$r.squared
n <- nrow(capm5)
S <- n * R2
(p_value <- 1 - pchisq(S, df = 1))
cat('R squared=',R2,'\nnRsquared=',S,'\np-value=',p_value,'\nConclusion:\nAt the 5% level, we do not reject the null hypothesis that the instruments are valid. Both RANK and POS are considered exogenous.' )

#10.24
#a
data(mroz)
mroz_lfp <- subset(mroz, lfp == 1)
mroz_lfp$lwage <- log(mroz_lfp$wage)
mroz_lfp$exper2 <- mroz_lfp$exper^2
library(AER)
iv_model_base <- ivreg(lwage ~ exper + exper2 + educ |
                         exper + exper2 + mothereduc + fathereduc,
                       data = mroz_lfp)
mroz_lfp$e_iv <- resid(iv_model_base)
plot(mroz_lfp$exper, mroz_lfp$e_iv,
     main = "IV Residuals vs Experience",
     xlab = "EXPER", ylab = "IV Residuals (e_IV)",
     pch = 20, col = "blue")
abline(h = 0, col = "red")
cat('The residual plot shows signs of heteroskedasticity. The spread of residuals is larger for women with lower levels of experience, and contracts as experience increases.')

#b
mroz_lfp$e_iv2 <- mroz_lfp$e_iv^2
aux_model <- lm(e_iv2 ~ exper, data = mroz_lfp)
R2 <- summary(aux_model)$r.squared
n <- nrow(mroz_lfp)
nR2 <- n * R2
p_value <- 1 - pchisq(nR2, df = 1)
cat("nR_squared =", round(nR2, 3), "p-value =", round(p_value, 4), "\n")
cat('Conclusion:\nThe test yielded a p-value of 0.0064, which is below the 5% significance level. Thus, we reject the null hypothesis of homoskedasticity and conclude that there is evidence of heteroskedasticity in the IV model.')

#c
library(sandwich)
library(lmtest)
robust_se <- vcovHC(iv_model_base, type = "HC1")
coeftest(iv_model_base, vcov = robust_se)

b_educ <- coef(iv_model_base)["educ"]
se_educ <- sqrt(robust_se["educ", "educ"])
ci_lower <- b_educ - 1.96 * se_educ
ci_upper <- b_educ + 1.96 * se_educ

baseline_se <- coeftest(iv_model_base, vcov = vcov(iv_model_base))[, 2]
robust_se_vec <- sqrt(diag(robust_se))
estimates <- coef(iv_model_base)
se_comparison <- data.frame(
  Estimate     = round(estimates, 5),
  Baseline_SE  = round(baseline_se, 5),
  Robust_SE    = round(robust_se_vec, 5),
  Increased_SE = ifelse(robust_se_vec > baseline_se, "Yes", "No")
)

print(se_comparison)
cat('Conclusion:\nAs shown in the table, all robust standard errors are larger than their baseline counterparts. This is consistent with the presence of heteroskedasticity, which inflates the variability of the estimators when not properly accounted for.')
cat("95% Robust CI for EDUC: [", round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")

#d
library(boot)
boot_iv <- function(data, indices) {
  d <- data[indices, ]  
  model <- ivreg(lwage ~ exper + exper2 + educ |
                   exper + exper2 + mothereduc + fathereduc,
                 data = d)
  return(coef(model))
}
set.seed(123)
boot_result <- boot(data = mroz_lfp, statistic = boot_iv, R = 200)
boot_se <- apply(boot_result$t, 2, sd)
se_compare <- data.frame(
  Coef         = names(baseline_se),
  Baseline_SE  = round(baseline_se, 5),
  Robust_SE    = round(robust_se_vec, 5),
  Bootstrap_SE = round(boot_se, 5),
  Larger_than_Baseline_SE = ifelse(boot_se > baseline_se, "Yes", "No"),
  Larger_than_Robust_SE = ifelse(boot_se > robust_se_vec, "Yes", "No")
)
print(se_compare)
(boot_ci_educ <- boot.ci(boot_result, type = "norm", index = 4))

