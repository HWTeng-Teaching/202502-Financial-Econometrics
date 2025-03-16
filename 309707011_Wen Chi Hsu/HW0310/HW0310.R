#3.7
#d
qt(0.99, df = 62)

#3.7
#b
BACHELOR <- seq(10, 50, by = 1)  
INCOME <- 11.52 + 1.029 * BACHELOR

plot(BACHELOR, INCOME, type="l", col="blue", lwd=2, 
     xlab="Percentage with Bachelor's Degree", 
     ylab="Income per Capita (in $1000s)", 
     main="Estimated Relationship Between Education and Income")

#e
library(ggplot2)

x <- seq(-4, 4, length = 1000)
y <- dt(x, df = 49)  

t_crit <- qt(0.975, df = 49)  
t_stat <- 0.572  
t_stat

#f
b2 <- 1.029  
se_b2 <- 0.0957
df <- 49 
alpha <- 0.01  

t_crit <- qt(1 - alpha/2, df)  

lower_bound <- b2 - t_crit * se_b2
upper_bound <- b2 + t_crit * se_b2

cat("99% Confidence Interval for Slope:", round(lower_bound, 3), "to", round(upper_bound, 3), "\n")

#3.17
#a
t_urban <- qt(0.95, df = 984)
t_urban

#b
t_rural <- qt(0.975, df = 212)
t_rural

#d
t_d <- qt(0.01, df = 212)
t_d

#3.19
#a
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
data("motel")
library(ggplot2)

ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel")) +
  geom_line(aes(y = comp_pct, color = "COMP")) +
  labs(title = "Occupancy Rates Over Time", x = "Time", y = "Occupancy (%)") +
  scale_color_manual(values = c("Motel" = "blue", "COMP" = "red")) +
  theme_minimal()

mod1 <- lm(motel_pct ~ comp_pct, data = motel)
summary(mod1)
ci1 <- confint(mod1, level = 0.95)
ci1

#b
ci2 <- data.frame(comp_pct = 70)
predict(mod1, newdata = ci2, interval = "confidence",level = 0.9)

#c
t_motel <- qt(0.99, df = 23)
t_motel

#d
t_motel2 <- qt(0.995, df = 23,lower.tail = F)
t_motel2

#e
motel$residuals <- residuals(mod1)

# Plot residuals against TIME
ggplot(motel, aes(x = time, y = residuals)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals of MOTEL_PCT Regression on COMP_PCT",
       x = "TIME",
       y = "Residuals") +
  geom_vline(xintercept = 17, color = "red", linewidth = 1) +
  geom_vline(xintercept = 23, color = "red", linewidth = 1) 
  theme_minimal()
  
  