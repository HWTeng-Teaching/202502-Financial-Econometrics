
# Define panel structure: SCHID = school ID, ID = student ID
pdata <- pdata.frame(star, index = c("SCHID", "ID"))

### a. OLS regression (no effects)
model_ols <- lm(READSCORE ~ SMALL + AIDE + TCHEXPER + BOY + WHITE_ASIAN + FREELUNCH, data = pdata)
summary(model_ols)

### b. Fixed effects by school
model_fe <- plm(READSCORE ~ SMALL + AIDE + TCHEXPER + BOY + WHITE_ASIAN + FREELUNCH,
                data = pdata, model = "within", effect = "individual")  # fixed effect by school
summary(model_fe)

### c. Test for fixed effects (F-test)
pFtest(model_fe, model_ols)

# Interpretation:
# If significant, school effects matter. Coefficient stability suggests low correlation with school fixed effects.

### d. Random effects model
model_re <- plm(READSCORE ~ SMALL + AIDE + TCHEXPER + BOY + WHITE_ASIAN + FREELUNCH,
                data = pdata, model = "random")
summary(model_re)

# Breusch-Pagan LM test for random effects vs. pooled OLS
plmtest(READSCORE ~ SMALL + AIDE + TCHEXPER + BOY + WHITE_ASIAN + FREELUNCH,
        data = pdata, effect = "individual", type = "bp")

### e. Hausman test for RE vs. FE
# Focus on SMALL, AIDE, TCHEXPER, WHITE_ASIAN, FREELUNCH
hausman_result <- phtest(model_fe, model_re)
print(hausman_result)

# What about BOY only?
model_fe_boy <- plm(READSCORE ~ BOY, data = pdata, model = "within")
model_re_boy <- plm(READSCORE ~ BOY, data = pdata, model = "random")
phtest(model_fe_boy, model_re_boy)

### f. Mundlak test (include school-means of covariates to test RE validity)

# Step 1: Create school means for covariates
school_means <- pdata %>%
  group_by(SCHID) %>%
  summarise(across(c(SMALL, AIDE, TCHEXPER, BOY, WHITE_ASIAN, FREELUNCH), mean, .names = "mean_{col}"))

# Merge school means back into pdata
pdata_mundlak <- left_join(as.data.frame(pdata), school_means, by = "SCHID")
pdata_mundlak <- pdata.frame(pdata_mundlak, index = c("SCHID", "ID"))

# Step 2: Mundlak model
mundlak_model <- plm(
  READSCORE ~ SMALL + AIDE + TCHEXPER + BOY + WHITE_ASIAN + FREELUNCH +
    mean_SMALL + mean_AIDE + mean_TCHEXPER + mean_BOY + mean_WHITE_ASIAN + mean_FREELUNCH,
  data = pdata_mundlak, model = "random"
)
summary(mundlak_model)

# Test joint significance of school-means (Wald test)
waldtest(
  mundlak_model,
  . ~ . - mean_SMALL - mean_AIDE - mean_TCHEXPER - mean_BOY - mean_WHITE_ASIAN - mean_FREELUNCH,
  vcov = vcovHC(mundlak_model, type = "HC1")
)

# Interpretation:
# If school-level means are significant, this implies correlation with school-specific effects,
# and random effects may be invalid. Prefer fixed effects in that case.
