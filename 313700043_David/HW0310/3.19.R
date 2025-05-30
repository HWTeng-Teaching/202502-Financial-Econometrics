rm(list = ls())  
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/motel.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
motel

#3.19 a

ggplot(motel, aes(x = time)) +
  geom_line(aes(y = motel_pct, color = "Motel Occupancy")) +
  geom_line(aes(y = comp_pct, color = "Competitor Occupancy"), linetype = "dashed") +
  labs(title = "Motel and Competitor Occupancy Rates Over Time",
       x = "Time (Months)", y = "Occupancy Rate (%)") +
  scale_color_manual(values = c("Motel Occupancy" = "blue", "Competitor Occupancy" = "red")) +
  theme_minimal()

model <- lm(motel_pct ~ comp_pct, data = motel)
summary(model)
confint(model, level = 0.95)
  
#3.19 b

new_data <- data.frame(comp_pct = 70)
prediction <- predict(lm(motel_pct ~ comp_pct, data = motel), 
                      newdata = new_data, 
                      interval = "confidence", 
                      level = 0.90)
print(prediction)

#3.19 e

motel$residuals <- residuals(model)
ggplot(motel, aes(x = time, y = residuals)) +
  geom_point() +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # 加上零殘差的基準線
  labs(title = "Residuals vs. Time",
       x = "Time (Months)",
       y = "Residuals") +
  theme_minimal()

# 檢查時間範圍 17-23 的殘差
subset_residuals <- motel[motel$time %in% 17:23, c("time", "residuals")]
print(subset_residuals)
plot(subset_residuals)
install.packages("lmtest")
library(lmtest)
bptest(model)
dwtest(model)


