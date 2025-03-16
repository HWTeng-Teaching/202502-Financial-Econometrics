#3.19
# a

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata"
load(url(url))


library(ggplot2)

ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy")) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy")) +
  labs(title = "Motel and Competitor Occupancy Over Time",
       x = "Time",
       y = "Occupancy Percentage",
       color = "Legend") +
  theme_minimal()

# It seems that two variables tend to move together.


model <- lm(motel_pct ~ comp_pct, data = motel)

summary(model)
#             Estimate Std. Error 
# (Intercept)  21.4000    12.9069    
# comp_pct      0.8646     0.2027

confint(model, "comp_pct", level = 0.95)
# [0.4452978, 1.283981]


# b

new_data <- data.frame(comp_pct = 70)

predict(model, newdata = new_data, interval = "confidence", level = 0.90)
# [77.38223, 86.46725]

