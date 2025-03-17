install.packages("POE5Rdata")
install.packages("ggplot2")
library(POE5Rdata)
library(ggplot2)

#3.19
#(a)
data(motel)

ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy")) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy")) +
  labs(title = "Motel vs Competitor Occupancy Over Time",
       x = "Time",
       y = "Occupancy Percentage",
       color = "Legend") +
  theme_minimal()

model = lm(motel_pct ~ comp_pct, data = motel)
summary(model)

conf_int = confint(model, level = 0.95)
print(conf_int)

#(b)
new_data = data.frame(comp_pct = 70)
pred_conf = predict(model, newdata = new_data, interval = "confidence", level = 0.90)
print(pred_conf)


#(e)
motel$residuals = residuals(model)

ggplot(motel, aes(x = time, y = residuals)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time",
       x = "Time",
       y = "Residuals") +
  theme_minimal()
