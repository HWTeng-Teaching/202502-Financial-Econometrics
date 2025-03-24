rm(list = ls())  
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
cex5_small

#4.29 a

#food
mean_food <- mean(cex5_small$food, na.rm = TRUE)  
median_food <- median(cex5_small$food, na.rm = TRUE)  
maximum_food <- max(cex5_small$food, na.rm = TRUE)
minimum_food <- min(cex5_small$food, na.rm = TRUE)
sd_food <- sd(cex5_small$food, na.rm = TRUE)

ggplot(cex5_small, aes(x = food)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 5) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  geom_vline(aes(xintercept = mean_food), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_food), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Food Expenditure", x = "Food Expenditure", y = "Frequency") +
  # Annotate mean and median values on the plot
  annotate("text", x = mean_food, y = 250 , label = paste("Mean =", round(mean_food, 2)),
           vjust = -0.5, color = "red") +
  annotate("text", x = median_food, y = 280 , label = paste("Median =", round(median_food, 2)),
           vjust = -1.5, color = "blue") +
  theme_minimal()
table1 <- data.frame(mean_food, median_food, maximum_food, minimum_food, sd_food)
kable(table1)

#income
mean_income <- mean(cex5_small$income, na.rm = TRUE)  
median_income <- median(cex5_small$income, na.rm = TRUE)  
maximum_income <- max(cex5_small$income, na.rm = TRUE)
minimum_income <- min(cex5_small$income, na.rm = TRUE)
sd_income <- sd(cex5_small$income, na.rm = TRUE)

ggplot(cex5_small, aes(x = income)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 5) +
  geom_vline(aes(xintercept = mean_income), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_income), color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Income Expenditure", x = "Income", y = "Frequency") +
  # Annotate mean and median values on the plot
  annotate("text", x = mean_income, y = 140 , label = paste("Mean =", round(mean_income, 2)),
           vjust = -0.5, color = "red") +
  annotate("text", x = median_income, y = 150 , label = paste("Median =", round(median_income, 2)),
           vjust = -1.5, color = "blue") +
  theme_minimal()

table2 <- data.frame(mean_income, median_income, maximum_income, minimum_income, sd_income)
kable(table2)

if (!require("tseries")) {
  install.packages("tseries")
  library(tseries)
}
# Jarque-Bera test for Food
jb_food <- jarque.bera.test(cex5_small$food)
print(jb_food)
# Jarque-Bera test for INCOME
jb_income <- jarque.bera.test(cex5_small$income)
print(jb_income)


#4.29 b

model1 <- lm(food~income, cex5_small)
summary(model1)
ggplot(cex5_small, aes(x = income, y = food)) +
  geom_point(color = "grey", alpha = 0.5)+
  geom_smooth(method = "lm", formula = food ~ income , color = "blue", size = 1 )+
  labs(
    title = "Food = 88.5665 + 0.3587*Income",
    x = "Income ($100)", 
    y = "Food Expenditure"
  )+
  theme_minimal()
confint(model1, level = 0.95)  

#4.29 c

predicted_food <- fitted(model1)  
residuals1 <- cex5_small$food - predicted_food
ggplot(cex5_small, aes(x = income, y = residuals1))+
  geom_point(color = "black", alpha = 0.5)+
  labs(
    title = "Residuals from Model 1",
    x = "Income($100)",
    y = "Residuals"
  )+
  theme_minimal()
ggplot(cex5_small, aes(x = residuals1)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 5)+
  labs(
    title = "Residuals Histogram for Model 1" , 
    x = "Residuals" , 
    y = "Frequency"
  )+
  theme_minimal()

jb.res1 <- jarque.bera.test(residuals1)
print(jb.res1)

#4.29 d

confint(model1, "income", level = 0.95)  

#4.29 e

model2 <- lm(log(food)~log(income), cex5_small)
summary(model2)
ggplot(cex5_small, aes(x = log(income), y = log(food)) ) +
  geom_point(color = "grey", alpha = 0.5)+
  geom_smooth(method = "lm", fomula = log(food)~log(income), color = "blue", size = 1 )+
  labs(
    title = "ln(Food) = 3.7789 + 0.1863*ln(Income)",
    x = "ln(Income) ($100)", 
    y = "ln(Food Expenditure)"
  )+
  theme_minimal()

#4.29 f

confint(model2, "log(income)", level = 0.95)  

#4.29 g

predicted_lnfood <- fitted(model2)  
residuals2 <- cex5_small$food - predicted_lnfood
ggplot(cex5_small, aes(x = log(income), y = residuals2))+
  geom_point(color = "black", alpha = 0.5)+
  labs(
    title = "Residuals from Model 2",
    x = "ln(Income) ($100)",
    y = "Residuals"
  )+
  theme_minimal()
ggplot(cex5_small, aes(x = residuals2)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 5)+
  labs(
    title = "Residuals Histogram for Model 2" , 
    x = "Residuals" , 
    y = "Frequency"
  )+
  theme_minimal()

jb.res2 <- jarque.bera.test(residuals2)
print(jb.res2)

#4.29 h

model3 <- lm(food~log(income), cex5_small)
summary(model3)
ggplot(cex5_small, aes(x = log(income), y = food)) +
  geom_point(color = "grey", alpha = 0.5)+
  geom_smooth(method = "lm", formula = food ~ log(income), color = "blue", size = 1 )+
  labs(
    title = "Food = 23.568 + 22.187*ln(Income)",
    x = "ln(Income) ($100)", 
    y = "Food Expenditure"
  )+
  theme_minimal()

#4.29 i

confint(model3, "log(income)", level = 0.95)

#4.29 j

predicted_food3 <- fitted(model3)  
residuals3 <- cex5_small$food - predicted_food3
ggplot(cex5_small, aes(x = log(income), y = residuals3))+
  geom_point(color = "black", alpha = 0.5)+
  labs(
    title = "Residuals from Model 3",
    x = "ln(Income) ($100)",
    y = "Residuals"
  )+
  theme_minimal()
ggplot(cex5_small, aes(x = residuals3)) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 5)+
  labs(
    title = "Residuals Histogram for Model 3" , 
    x = "Residuals" , 
    y = "Frequency"
  )+
  theme_minimal()

jb.res3 <- jarque.bera.test(residuals3)
print(jb.res3)

