library(POE5Rdata)
data("motel")
?motel
library(ggplot2)

#a
ggplot(data=motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy")) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy")) +
  labs(x = "Time",
       y = "Occupancy Percentage") +
  scale_color_manual( values = c("Motel Occupancy" = "blue", "Competitor Occupancy" = "red")) +
  theme_minimal()


model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)

confint(model, level = 0.95)

#b
new_data <- data.frame(comp_pct = 70)
predict(model, newdata = new_data, interval = "confidence", level = 0.90)

#c
model<-lm(motel_pct~ comp_pct,data=motel)
summary(model)

coef(summary(model))
n<-nrow(motel)
df<-n-2

t_critical<-qt(0.99,df)
t_critical

#d
model<-lm(motel_pct~ comp_pct,data=motel)
summary(model)

beta_hat<-coef(model)["comp_pct"]
se_beta_hat <- coef(summary(model))["comp_pct", "Std. Error"]

t_value <- (beta_hat - 1) / se_beta_hat
t_value

n <- nrow(motel)
df <- n - 2

t_critical <- qt(0.995, df)
t_critical

#e
residuals <- residuals(model)
motel$residuals <- residuals

library(ggplot2)

ggplot(motel, aes(x = time, y = residuals)) +
  geom_line(color = 'blue') +  
  geom_hline(yintercept = 0, color = 'red', linetype = "dashed") +  
  geom_vline(xintercept = 17, color = "red", linetype = "solid") +  
  geom_vline(xintercept = 23, color = "red", linetype = "solid") +  
  labs(x = "Month (1 = March 2003)",
       y = "Residuals") +
  theme_minimal()