#4.4 (a)
# Define experience years from 0 to 30
EXPER <- seq(0, 30, by = 1)

# Model 1 fitted values: RATING = 64.289 + 0.990 * EXPER
model_1 <- 64.289 + 0.990 * EXPER

# Plotting
plot(EXPER, model_1, type = "l", col = "purple", xlab = "Experience (Years)", ylab = "Rating", main = "Model 1 Fitted Values")

#4.4 (b)
# Model 2 fitted values: RATING = 39.464 + 15.312 * log(EXPER)
model_2 <- 39.464 + 15.312 * log(EXPER)

# Plotting
plot(EXPER, model_2, type = "l", col = "purple", xlab = "Experience (Years)", ylab = "Rating", main = "Model 2 Fitted Values")

#4.4 (c)
# Marginal effect for Model 1
marginal_effect_model_1 <- 0.990
marginal_effect_10_years_1 <- marginal_effect_model_1
marginal_effect_20_years_1 <- marginal_effect_model_1

#4.4 (d)
# Marginal effect for Model 2: d(RATING)/d(EXPER) = 15.312 / EXPER
marginal_effect_model_2_10 <- 15.312 / 10
marginal_effect_model_2_20 <- 15.312 / 20

