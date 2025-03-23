#4.4
# 313707014 陳紀蓁

#a. model_1 圖
library(ggplot2) 

experience <- seq(0, 30, by = 1)

rating <- 64.289 + 0.990 * experience

df <- data.frame(experience, rating)

ggplot(df, aes(x = experience, y = rating)) +
  geom_line(color = "blue", size = 1) +  
  geom_point(color = "black", size = 2) +  
  labs(title = "Fitted Values from Model 1",
       x = "Years of Experience",
       y = "Predicted Rating") +
  theme_minimal()  


# model 2 圖

experience <- seq(1, 30, by = 1)

rating <- 39.464 + 15.312 * log(experience)

df <- data.frame(experience, rating)

ggplot(df, aes(x = experience, y = rating)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "black", size = 2) +  
  labs(title = "Fitted Values from Model 2",
       x = "Years of Experience",
       y = "Predicted Rating") +
  theme_minimal()  



