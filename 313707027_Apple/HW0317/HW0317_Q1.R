# 4.04
# (a)
EXPER <- 0:30
RATING_model1 <- 64.289 + 0.990 * EXPER
plot(EXPER, RATING_model1, type = "l", col = "blue", lwd = 2, 
     xlab = "EXPER", ylab = "RATING", 
     main = "Model 1")

# (b)
EXPER <- 1:30
RATING_model2 <- 39.464 + 15.312 * log(EXPER)
plot(EXPER, RATING_model2, type = "l", col = "red", lwd = 2, 
     xlab = "EXPER", ylab = "RATING", 
     main = "Model 2")

# (d)
marginal_effect_model2 <- function(EXPER) {
  15.312 / EXPER
}
cat("marginal effect Model 2(EXPER=10): ",
    marginal_effect_model2(10), "\n")
cat("marginal effect Model 2(EXPER=10): ",
    marginal_effect_model2(20), "\n")

# (e)
R2_model1 <- 0.3793
R2_model2 <- 0.6414
R2_model1_experienced <- 0.4858
comparison <- data.frame(Model = c("Model 1", "Model 2", "Model 1 (Experienced Only)"),
                         R2 = c(R2_model1, R2_model2, R2_model1_experienced))
print(comparison)