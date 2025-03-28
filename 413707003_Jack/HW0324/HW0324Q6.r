# 5.23
# c

summary(model)$r.squared
#  0.50965


# d

# H_0:beta_2>=0
# H_1:beta_2<0

qt(0.05, 52, lower.tail = TRUE)
#  -1.674689

# Since -5.892 < -1.674689, we reject H_0, indicating that an increase in quantity leads to a decrease in price.




