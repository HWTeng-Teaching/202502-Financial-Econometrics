# 2.25
# (a)
data('cex5_small')
str(cex5_small)
length(cex5_small$foodaway)
summary(cex5_small$foodaway)
hist(cex5_small$foodaway,
     breaks=30,xlab = 'FOODAWAY',ylab = "Frequency",
     col = rgb(0.1, 0.6, 0.8, 0.7),
     main = 'Histogram of FOODAWAY')
# (b)
cex5_small$advanced <- as.logical(cex5_small$advanced)
advanced <- cex5_small$foodaway[cex5_small$advanced]
length(advanced)
summary(advanced)

cex5_small$college <- as.logical(cex5_small$college)
college <- cex5_small$foodaway[cex5_small$college]
length(college)
summary(college)

ncorna <- cex5_small$foodaway[cex5_small$college==F&cex5_small$advanced==F]
length(ncorna)
summary(ncorna)

# (c)
cex5_small <- cex5_small[cex5_small$foodaway>0,]
cex5_small$ln_foodaway <- log(cex5_small$foodaway)
length(cex5_small$ln_foodaway)
summary(cex5_small$ln_foodaway)
hist(cex5_small$ln_foodaway, 
     breaks = 30, xlab = "ln(FOODAWAY)", ylab = "Frequency", 
     col = rgb(0.1, 0.6, 0.8, 0.7),  
     main = "Histogram of ln(FOODAWAY)")

# (d)
model <- lm(ln_foodaway ~ income, data = cex5_small)
summary(model)

# (e)
plot(cex5_small$income,cex5_small$lnfoodaway,
     pch=16, col = "blue", cex = 0.5,
     xlab = 'INCOME',ylab = 'ln(FOODAWAY)',
     main = "Observations and log-linear fitted line")
abline(model, col = "red",lwd=1.5)

# (f)
resid_square <- sum(summary(model)$resid)
plot(cex5_small$income,summary(model)$resid,pch=16,cex=0.5)

residuals <- residuals(model)
plot(cex5_small$income, residuals,
     main = "Residuals vs. INCOME",  
     xlab = "Income", ylab = "Residuals", 
     pch = 16, col = "blue",cex = 0.5)
