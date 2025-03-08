# e

ggplot(cex5_small, aes(x = income, y = ln_foodaway)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot of ln(foodaway) vs income",
       x = "Household Monthly Income ($100 units)",
       y = "ln(Food Away from Home Expenditure)") +
  theme_minimal()


# f

cex5_small_clean <- cex5_small[!is.na(cex5_small$ln_foodaway) & !is.nan(cex5_small$ln_foodaway) & is.finite(cex5_small$ln_foodaway), ]
lm_model <- lm(ln_foodaway ~ income, data = cex5_small_clean)
residuals <- resid(lm_model)
cex5_small_clean$residuals <- residuals
cex5_small$residuals <- NA
cex5_small$residuals[cex5_small$education_group %in% cex5_small_clean$education_group] <- cex5_small_clean$residuals


ggplot(cex5_small, aes(x = income, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of the Regression Model",
       x = "Household Monthly Income ($100 units)",
       y = "Residuals") +
  theme_minimal()


# It seems that the residuals follow a random distribution, without any unusual patterns.



