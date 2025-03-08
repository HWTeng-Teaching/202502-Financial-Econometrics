# c

cex5_small$ln_foodaway <- log(cex5_small$foodaway)

ggplot(cex5_small, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of ln(foodaway)",
       x = "ln(Food Away from Home Expenditure)",
       y = "Frequency") +
  theme_minimal()

summary(cex5_small$ln_foodaway, na.rm = TRUE)

# Since there are zero values in foodaway, taking the natural logarithm of foodaway results in undefined values, which is why ln(foodaway) has fewer observations than foodaway.


# d 

cex5_small_clean <- cex5_small[!is.na(cex5_small$ln_foodaway) & !is.nan(cex5_small$ln_foodaway) & is.finite(cex5_small$ln_foodaway), ]

lm_model <- lm(ln_foodaway ~ income, data = cex5_small_clean)

summary(lm_model)

# (income increase 100 -> foodaway increase 0.69%)
