# 15.20
# d

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"))

re_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = star, index = c("schid", "id"), model = "random")
summary(re_model)

#               Estimate Std. Error  z-value  Pr(>|z|)    
# (Intercept) 436.126774   2.064782 211.2217 < 2.2e-16 ***
# small         6.458722   0.912548   7.0777 1.466e-12 ***
# aide          0.992146   0.881159   1.1260    0.2602    
# tchexper      0.302679   0.070292   4.3060 1.662e-05 ***
# boy          -5.512081   0.727639  -7.5753 3.583e-14 ***
# white_asian   7.350477   1.431376   5.1353 2.818e-07 ***
# freelunch   -14.584332   0.874676 -16.6740 < 2.2e-16 ***

# The significance and direction remain unchanged.

plmtest(re_model, type = "bp")

# Because p < 0.05, we reject H_0, indicating the presence of school-level variation, and the random effects model is appropriate.









