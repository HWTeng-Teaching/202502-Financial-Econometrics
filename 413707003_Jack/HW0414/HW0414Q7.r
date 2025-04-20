# 8.18
# d

robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
#                  Estimate  Std. Error  t value  Pr(>|t|)    
#   (Intercept)  1.2014e+00  3.2794e-02  36.6340 < 2.2e-16 ***
#   educ         1.0123e-01  1.9058e-03  53.1160 < 2.2e-16 ***
#   exper        2.9622e-02  1.3149e-03  22.5276 < 2.2e-16 ***
#   I(exper^2)  -4.4578e-04  2.7597e-05 -16.1533 < 2.2e-16 ***
#   female      -1.6550e-01  9.4883e-03 -17.4428 < 2.2e-16 ***
#   black       -1.1153e-01  1.6094e-02  -6.9297 4.482e-12 ***
#   metro        1.1902e-01  1.1582e-02  10.2762 < 2.2e-16 ***
#   south       -4.5755e-02  1.3902e-02  -3.2914  0.001001 ** 
#   midwest     -6.3943e-02  1.3724e-02  -4.6591 3.217e-06 ***
#   west        -6.5891e-03  1.4557e-02  -0.4526  0.650813  

#convention
  #               Estimate Std. Error t value Pr(>|t|)    
  # (Intercept)  1.201e+00  3.211e-02  37.409  < 2e-16 ***
  # educ         1.012e-01  1.758e-03  57.574  < 2e-16 ***
  # exper        2.962e-02  1.300e-03  22.780  < 2e-16 ***
  # I(exper^2)  -4.458e-04  2.635e-05 -16.915  < 2e-16 ***
  # female      -1.655e-01  9.529e-03 -17.368  < 2e-16 ***
  # black       -1.115e-01  1.694e-02  -6.583 4.86e-11 ***
  # metro        1.190e-01  1.231e-02   9.671  < 2e-16 ***
  # south       -4.576e-02  1.356e-02  -3.374 0.000744 ***
  # midwest     -6.394e-02  1.410e-02  -4.534 5.86e-06 ***
  # west        -6.589e-03  1.440e-02  -0.458 0.647321 

# narrower:female, black, metro, midwest
# wider:educ, exper, exper^2, south, west
# No, there is no inconsistency in the results.


# e

aux_model <- lm(residuals_squared ~ metro + exper, data = cps5)

cps5$hatsigma2 <- fitted(aux_model)
cps5$weights <- 1 / cps5$hatsigma2
fgls_model <- lm(log(wage) ~ educ + exper + I(exper^2) + female + black + metro + south + midwest + west, data = cps5, weights = weights)
ci_e <- confint(fgls_model)
interval_length <- ci_e[, 2] - ci_e[, 1]
data.frame(Estimate = coef(fgls_model),
           Lower = ci_e[, 1],
           Upper = ci_e[, 2],
           CI_Length = interval_length)
#                  Estimate         Lower         Upper    CI_Length
# (Intercept)  1.2089331351  1.1465031674  1.2713631028 0.1248599354
# educ         0.1008630707  0.0974071491  0.1043189922 0.0069118430
# exper        0.0294158328  0.0268745523  0.0319571133 0.0050825609
# I(exper^2)  -0.0004428831 -0.0004943719 -0.0003913943 0.0001029776
# female      -0.1661539431 -0.1847233999 -0.1475844864 0.0371389135
# black       -0.1102081035 -0.1417849407 -0.0786312662 0.0631536744
# metro        0.1198985331  0.0972010590  0.1425960072 0.0453949482
# south       -0.0461687019 -0.0726804224 -0.0196569813 0.0530234411
# midwest     -0.0645282735 -0.0920349516 -0.0370215953 0.0550133563
# west        -0.0061871672 -0.0344710343  0.0220966998 0.0565677341

vcov_hc <- vcovHC(model, type = "HC1")
ci_d <- coefci(model, vcov. = vcov_hc, level = 0.95)
interval_length <- ci_d[, 2] - ci_d[, 1]
data.frame(Estimate = coef(model),
           Lower = ci_d[, 1],
           Upper = ci_d[, 2],
           CI_Length = interval_length)
#                  Estimate        Lower         Upper    CI_Length
# (Intercept)  1.2013820235  1.137098683  1.2656653641 0.1285666813
# educ         0.1012296134  0.097493811  0.1049654160 0.0074716051
# exper        0.0296216959  0.027044205  0.0321991870 0.0051549824
# I(exper^2)  -0.0004457805 -0.000499876 -0.0003916849 0.0001081911
# female      -0.1655019802 -0.184100928 -0.1469030324 0.0371978956
# black       -0.1115252498 -0.143072211 -0.0799782888 0.0630939222
# metro        0.1190204104  0.096316998  0.1417238226 0.0454068242
# south       -0.0457554333 -0.073005508 -0.0185053588 0.0545001491
# midwest     -0.0639432877 -0.090845662 -0.0370409129 0.0538047496
# west        -0.0065891022 -0.035123519  0.0219453146 0.0570688335

# narrower:educ, exper, exper^2, south, west
# wider:female, black, metro, midwest


# f

ci_f <- coefci(fgls_model, vcov. = vcov_hc, level = 0.95)
interval_length <- ci_f[, 2] - ci_f[, 1]
data.frame(Estimate = coef(fgls_model),
           Lower = ci_f[, 1],
           Upper = ci_f[, 2],
           CI_Length = interval_length)
#                  Estimate         Lower         Upper    CI_Length
# (Intercept)  1.2089331351  1.1446497944  1.2732164757 0.1285666813
# educ         0.1008630707  0.0971272681  0.1045988732 0.0074716051
# exper        0.0294158328  0.0268383416  0.0319933240 0.0051549824
# I(exper^2)  -0.0004428831 -0.0004969786 -0.0003887875 0.0001081911
# female      -0.1661539431 -0.1847528909 -0.1475549953 0.0371978956
# black       -0.1102081035 -0.1417550646 -0.0786611424 0.0630939222
# metro        0.1198985331  0.0971951210  0.1426019452 0.0454068242
# south       -0.0461687019 -0.0734187764 -0.0189186273 0.0545001491
# midwest     -0.0645282735 -0.0914306483 -0.0376258987 0.0538047496
# west        -0.0061871672 -0.0347215840  0.0223472495 0.0570688335

# narrower:female, black, metro, midwest
# wider:educ, exper, exper^2, south, west


# g

# I would choose FGLS with robust standard errors. Because this model accounts for heteroskedasticity and uses more robust standard errors.















