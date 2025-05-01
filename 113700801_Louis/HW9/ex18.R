data("mroz")
df <- mroz %>% filter(inlf == 1)

### a. Create dummy variables and calculate percentage
df <- df %>%
  mutate(
    MOTHERCOLL = ifelse(motheduc > 12, 1, 0),
    FATHERCOLL = ifelse(fatheduc > 12, 1, 0)
  )

# Percentages
percent_mothercoll <- mean(df$MOTHERCOLL) * 100
percent_fathercoll <- mean(df$FATHERCOLL) * 100

cat("Percentage with mother having college education:", percent_mothercoll, "\n")
cat("Percentage with father having college education:", percent_fathercoll, "\n")

### b. Correlations
correlations <- cor(df %>% select(educ, MOTHERCOLL, FATHERCOLL), use = "complete.obs")
print(correlations)

# Logical argument: binary instruments like MOTHERCOLL and FATHERCOLL may reduce bias from measurement error
# and improve interpretability, while still being correlated with EDUC.

### c. IV regression with MOTHERCOLL as instrument
iv_model_c <- ivreg(log(wage) ~ educ + exper + expersq | MOTHERCOLL + exper + expersq, data = df)
summary(iv_model_c)
confint(iv_model_c, level = 0.95)

### d. First stage regression (EDUC on MOTHERCOLL)
first_stage_d <- lm(educ ~ MOTHERCOLL + exper + expersq, data = df)
summary(first_stage_d)
# F-test value
f_test <- summary(first_stage_d)$fstatistic
cat("F-statistic for MOTHERCOLL:", f_test[1], "\n")

### e. IV regression with both MOTHERCOLL and FATHERCOLL
iv_model_e <- ivreg(log(wage) ~ educ + exper + expersq | MOTHERCOLL + FATHERCOLL + exper + expersq, data = df)
summary(iv_model_e)
confint(iv_model_e, level = 0.95)

### f. First stage with both instruments
first_stage_f <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + expersq, data = df)
summary(first_stage_f)

# F-test for joint significance
anova_f <- anova(first_stage_f)
print(anova_f)

### g. Test for overidentifying restrictions (validity of surplus instrument)
# Hansen J test
library(sandwich)
library(lmtest)

# Use robust standard errors
j_test <- summary(iv_model_e, diagnostics = TRUE)
print(j_test$diagnostics)
