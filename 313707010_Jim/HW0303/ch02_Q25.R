library(POE5Rdata)
data("cex5_small")

# a
hist(cex5_small$foodaway, main="Histogram of FOODAWAY", xlab="FOODAWAY", col="skyblue", border="white")

summary(cex5_small$foodaway)
mean_foodaway <- mean(cex5_small$foodaway)
median_foodaway <- median(cex5_small$foodaway)
quantile_foodaway <- quantile(cex5_small$foodaway, probs=c(0.25, 0.75))

print(mean_foodaway)
print(median_foodaway)
print(quantile_foodaway)

# b
advanced_group <- subset(cex5_small, advanced == 1)
mean_advanced <- mean(advanced_group$foodaway)
median_advanced <- median(advanced_group$foodaway)

college_group <- subset(cex5_small, college == 1)
mean_college <- mean(college_group$foodaway)
median_college <- median(college_group$foodaway)

no_degree_group <- subset(cex5_small, advanced == 0 & college == 0)
mean_no_degree <- mean(no_degree_group$foodaway)
median_no_degree <- median(no_degree_group$foodaway)

mean_advanced
median_advanced

mean_college
median_college

mean_no_degree
median_no_degree

# c
cex5_small <- cex5_small[cex5_small$foodaway > 0, ]
cex5_small$ln_foodaway <- log(cex5_small$foodaway)
hist(cex5_small$ln_foodaway, main="Histogram of ln(FOODAWAY)", xlab="ln(FOODAWAY)", col="lightgreen", border="white")

summary(cex5_small$ln_foodaway)
mean_ln_foodaway <- mean(cex5_small$ln_foodaway)
median_ln_foodaway <- median(cex5_small$ln_foodaway)
quantile_ln_foodaway <- quantile(cex5_small$ln_foodaway, probs=c(0.25, 0.75))

mean_ln_foodaway
median_ln_foodaway
quantile_ln_foodaway

# d
model <- lm(ln_foodaway ~ income, data=cex5_small)
summary(model)

beta2 <- coef(model)["income"]
beta2

# e
plot(cex5_small$income, cex5_small$ln_foodaway, main="ln(FOODAWAY) vs INCOME", xlab="INCOME", ylab="ln(FOODAWAY)", pch=19, col="darkblue")
abline(model, col="red")

# f
residuals <- resid(model)
plot(cex5_small$income, residuals, main="Residuals vs INCOME", xlab="INCOME", ylab="Residuals", pch=19, col="purple")
