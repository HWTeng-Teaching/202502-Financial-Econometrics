# 15.20
# a

load(url("https://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"))

ols_model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(ols_model)

#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 437.76425    1.34622 325.180  < 2e-16 ***
# small         5.82282    0.98933   5.886 4.19e-09 ***
# aide          0.81784    0.95299   0.858    0.391    
# tchexper      0.49247    0.06956   7.080 1.61e-12 ***
# boy          -6.15642    0.79613  -7.733 1.23e-14 ***
# white_asian   3.90581    0.95361   4.096 4.26e-05 ***
# freelunch   -14.77134    0.89025 -16.592  < 2e-16 ***

# Yes.
# No.
# Yes.
# Yes. Both sex or race make a difference.


# b
library(plm)

star$schid <- as.factor(star$schid) 
star$id <- as.factor(star$id)

fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = star, index = c("schid", "id"), model = "within")
summary(fe_model)

#               Estimate Std. Error  t-value  Pr(>|t|)    
# small         6.490231   0.912962   7.1090 1.313e-12 ***
# aide          0.996087   0.881693   1.1297    0.2586    
# tchexper      0.285567   0.070845   4.0309 5.629e-05 ***
# boy          -5.455941   0.727589  -7.4987 7.440e-14 ***
# white_asian   8.028019   1.535656   5.2277 1.777e-07 ***
# freelunch   -14.593572   0.880006 -16.5835 < 2.2e-16 ***

# No. All my conclusions remain the same.


# c

pFtest(fe_model, ols_model)

# data:  readscore ~ small + aide + tchexper + boy + white_asian + freelunch
# F = 16.698, df1 = 78, df2 = 5681, p-value < 2.2e-16

#Because the p-value is less than 0.05, we reject the null hypothesis, indicating the presence of school-level heterogeneity.

