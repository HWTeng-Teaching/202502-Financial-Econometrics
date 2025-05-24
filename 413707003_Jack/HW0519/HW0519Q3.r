# 15.20
# e

hausman_result <- phtest(fe_model, re_model)
print(hausman_result)

# Because p < 0.05, we reject H_0, indicating that the random effects estimator is biased and the fixed effects model should be used.


# f

star$SMALL_MEAN     <- ave(star$small, star$schid, FUN = mean)
star$AIDE_MEAN      <- ave(star$aide, star$schid, FUN = mean)
star$FREELUNCH_MEAN <- ave(star$freelunch, star$schid, FUN = mean)


mundlak_model <- plm(readscore ~ tchexper + boy + white_asian + freelunch +
                       SMALL_MEAN + AIDE_MEAN + FREELUNCH_MEAN,
                     data = star, index = c("schid", "id"), model = "random")
summary(mundlak_model)

#                  Estimate Std. Error  z-value  Pr(>|z|)    
# (Intercept)    437.976250  11.100726  39.4547 < 2.2e-16 ***
# tchexper         0.289941   0.070414   4.1177 3.827e-05 ***
# boy             -5.508495   0.731194  -7.5336 4.937e-14 ***
# white_asian      7.629979   1.490626   5.1186 3.077e-07 ***
# freelunch      -14.677692   0.883711 -16.6092 < 2.2e-16 ***
# SMALL_MEAN     -15.203825  21.304668  -0.7136    0.4755    
# AIDE_MEAN       10.231029  19.520136   0.5241    0.6002    
# FREELUNCH_MEAN   2.839152   5.764797   0.4925    0.6224


# Because both SMALL\_MEAN and AIDE\_MEAN are not significant, the random effects model is appropriate.






