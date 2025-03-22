#-----------------------C03Q01-----------------------
#(d)
n = 64
alpha = 0.01
df = n - 2  
critical_value <- qt(1 - alpha, df)

b2 = 0.01309
se_b2 = 0.00215
ts = b2/se_b2

cat("t-statistic:", ts)
cat("Critical value at α =", alpha, ":", critical_value)


#-----------------------C03Q07-----------------------
#(a)
n = 51
se_b1 = 2.672
ts_b1 = 4.31
b1 = se_b1*ts_b1
#(b)
x <- seq(0, 10)
y <- 11.51632 + 1.029 * x
plot(x, y, type = "l", col = "blue", lwd = 2, 
     xlab = "BACHELOR", ylab = "INCOME", )
grid()
#(c)
b2 = 1.029
ts_b2 = 10.75
se_b2 = b2/ts_b2
#(d)
t = (b1-10)/se_b1
print(t)

#(e)
df = 51 - 1 
pv = pt(t, 51-1)
print(pv)
# t-distribution
x <- seq(-5, 5, length = 100)
y <- dt(x, df)

plot(x, y, type = "l", col = "blue", lwd = 2, 
     xlab = "t-statistic", ylab = "Density", 
     main = "Two-Tailed Test with p-value = 0.572")

area_left <- qt(0.025, df)  # Critical value for left tail
area_right <- qt(0.975, df)  # Critical value for right tail
polygon(c(-5, area_left, area_left), c(0, 0, dt(area_left, df)), col = "red", density = 20)
polygon(c(5, area_right, area_right), c(0, 0, dt(area_right, df)), col = "red", density = 20)

test_stat <- 0.572  # p-value corresponds to this test statistic, this is a rough estimate
points(test_stat, dt(test_stat, df), col = "green", pch = 19, cex = 1.5)
text(test_stat, dt(test_stat, df) + 0.02, labels = "p-value = 0.572", col = "green")
grid()

#(f)
alpha = 0.01
b2 = 1.029
ts_b2 = 10.75
se_b2 = b2/ts_b2
df = 51 - 2
t = qt(1-alpha/2, df)
cat("99% CI: [", b2-(t*se_b2), ", ", b2+(t*se_b2), "]")

#(g)
alpha = 0.05
b2 = 1.029
ts_b2 = 10.75
se_b2 = b2/ts_b2
df = 51 - 2

critical_value <- qt(1-alpha/2, df)
ts = (b2-1)/se_b2

cat("t-statistic:", ts)
cat("Critical value at α =", alpha, ": ±",critical_value)



#-----------------------C03Q17-----------------------
reg17 <- data.frame(
  N = c(986, 214),
  b1 = c(-10.76,-4.88),
  b2 = c(2.46,1.80),
  se_b1 = c(2.27,3.29),
  se_b2 = c(0.16,0.24)
)

#(a)
alpha = 0.05
df = reg17[1,"N"] - 2  
critical_value <- qt(1 - alpha, df)

ts = (reg17[1,"b2"]-1.8)/reg17[1,"se_b2"]

cat("t-statistic:", ts)
cat("Critical value at α =", alpha, ":", critical_value)

#(b)
alpha = 0.05
df = reg17[1,"N"] - 2
EDUC = 16
cov_b0b1 = -0.345
se_y = sqrt(reg17[1,"se_b1"]^2 + (EDUC*reg17[1,"se_b2"])^2 + 2*EDUC*cov_b0b1)
print(se_y)
t = qt(1-alpha/2, df)
y = reg17[1,"b1"]+reg17[1,"b2"]*EDUC
cat("95% CI: [", y-(t*se_y), ", ", y+(t*se_y), "]")
#(c)
alpha = 0.05
df = reg17[2,"N"] - 2
EDUC = 16
cov_b0b1 = -0.761
se_y = sqrt(reg17[2,"se_b1"]^2 + (EDUC*reg17[2,"se_b2"])^2 + 2*EDUC*cov_b0b1)
print(se_y)
t = qt(1-alpha/2, df)
y = reg17[1,"b1"]+reg17[1,"b2"]*EDUC
cat("95% CI: [", y-(t*se_y), ", ", y+(t*se_y), "]")
#(d)
alpha = 0.01
df = reg17[2,"N"] - 2  
critical_value <- qt(alpha, df)
ts = (reg17[2,"b1"]-4)/reg17[2,"se_b1"]

cat("t-statistic:", ts)
cat("Critical value at α =", alpha, ":", critical_value)



#-----------------------C03Q19-----------------------
library(POE5Rdata)
library(ggplot2)
data(motel)

#(a)
ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "MOTEL_PCT")) +  
  geom_line(aes(y = comp_pct, color = "COMP_PCT")) +    
  labs(
    title = "MOTEL_PCT and COMP_PCT versus TIME",
    x = "TIME",
    y = "Percentage",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("MOTEL_PCT" = "blue", "COMP_PCT" = "red"))  

mod <- lm(motel_pct ~ comp_pct, data = motel)
summary(mod)
CI95 <- confint(mod, level = 0.95)
CI95

#(b)
x <- list(comp_pct = 70)
pred <- predict(mod, x, interval = "prediction", level = 0.90)
pred

#(c)
n = nrow(motel)
alpha = 0.01
df = n - 2  
critical_value <- qt(1 - alpha, df)

ts = summary(mod)$coefficients[2,"t value"]

cat("t-statistic:", ts)
cat("Critical value at alpha =", alpha, ":", critical_value)

if (ts > critical_value) {
  cat("Reject H0: There is evidence that beta2 > 0.")
} else {
  cat("Fail to reject H0: There is insufficient evidence that beta2 > 0.")
}

#(d)
alpha = 0.01
cvl <- qt(alpha/2, df)
cvu <- qt(1 - alpha/2, df)

b2 = summary(mod)$coefficients[2,"Estimate"]
se_b2 = summary(mod)$coefficients[2,"Std. Error"]
ts = (b2-1)/se_b2

cat("t-statistic:", ts)
cat("Critical value at alpha =", alpha, ":", critical_value)

if (ts < cvl || ts >cvu) {
  cat("Reject H0: There is evidence that beta2 = 1.")
} else {
  cat("Fail to reject H0: There is insufficient evidence that beta2 = 1.")
}

#(e)
rs <- mod$residuals
plot(motel$time, rs, 
     main = "Residuals of MOTEL_PCT vs COMP_PCT",
     xlab = "Time", ylab = "Residuals", 
     pch = 16, col = "blue")

rs17_23 <- rs[17:23]
sum(rs17_23)
rs17_23
