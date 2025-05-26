# Load libraries
library(plm)
library(lmtest)
library(sandwich)
library(dplyr)

# Load your data: assumes 'star' contains kindergarten data
# Columns: READSCORE, SMALL, AIDE, TCHEXPER, BOY, WHITE_ASIAN, FREELUNCH, ID, SCHID
# star <- read.csv("star.csv")  # or load your data appropriately

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
