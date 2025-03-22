# 4.4
# a

exper <- 0:30
rating_model1 <- 64.289 + 0.990 * exper


plot(exper, rating_model1, type = "l", col = "blue", lwd = 2, 
     xlab = "Years of Experience", ylab = "Predicted Rating",
     main = "Fitted Values from Model 1")


# b

exper2 <- 1:30
rating_model2 <- 39.464 + 15.312 * log(exper2)

plot(exper2, rating_model2, type = "l", col = "red", lwd = 2, 
     xlab = "Years of Experience", ylab = "Predicted Rating",
     main = "Fitted Values from Model 2")


# Since Model 2 uses ln(EXPER), and ln(0) is undefined, the four artists with no experience were excluded.


# c
# (i)
# marginal effect(10y) = 0.990

# (ii)
# marginal effect(20y) = 0.990





