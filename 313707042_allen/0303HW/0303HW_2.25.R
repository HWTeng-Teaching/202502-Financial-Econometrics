library(POE5Rdata)
data("cex5_small")
?cex5_small

#a
library(ggplot2)
ggplot(cex5_small,aes(x=foodaway))+
  geom_histogram(binwidth = 20,fill="blue",color="black",alpha=0.7)+
  geom_vline(aes(xintercept = mean_value), color = "red", linetype = "dashed", linewidth = 1) +  # 均值
  geom_vline(aes(xintercept = median_value), color = "green", linetype = "dashed", linewidth = 1) +  # 中位數
  geom_vline(aes(xintercept = q1), color = "purple", linetype = "dashed", linewidth = 1) +  # 25th 百分位數
  geom_vline(aes(xintercept = q3), color = "orange", linetype = "dashed", linewidth = 1) +  # 75th 百分位數
  
  
  labs(x="FOODAWAY(Monthly Spending on foodaway from home per person)",
       y="Frequency")+
  theme_minimal()

mean_value<-mean(cex5_small$foodaway, na.rm=TRUE)
median_value<-median(cex5_small$foodaway,na.rm = TRUE)
q1<-quantile(cex5_small$foodaway,probs =0.25,na.rm=TRUE)
q3<-quantile(cex5_small$foodaway,probs =0.75,na.rm=TRUE)

print(mean_value)
print(median_value)
print(q1)
print(q3)

#b
advanced_degree<-subset(cex5_small,advanced==1)
college_degree<-subset(cex5_small, advanced==0&college==1)
neither_degree<-subset(cex5_small, advanced==0&college==0)

mean_adv <- mean(advanced_degree$foodaway, na.rm = TRUE)
median_adv <- median(advanced_degree$foodaway, na.rm = TRUE)

mean_college <- mean(college_degree$foodaway, na.rm = TRUE)
median_college <- median(college_degree$foodaway, na.rm = TRUE)

mean_neither <- mean(neither_degree$foodaway, na.rm = TRUE)
median_neither <- median(neither_degree$foodaway, na.rm = TRUE)

print(mean_adv)
print(median_adv)
print(mean_college)
print(median_college)
print(mean_neither)
print(median_neither)
#c
library(ggplot2)

cex5_small$ln_foodaway <- ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)

ggplot(data = cex5_small, aes(x = ln_foodaway)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of ln(FOODAWAY)",
       x = "ln(FOODAWAY)",
       y = "Frequency") +
  theme_minimal()

#d
cex5_small$ln_foodaway<-ifelse(cex5_small$foodaway>0,log(cex5_small$foodaway),NA)
lm_model<-lm(ln_foodaway~income,data=cex5_small)
summary(lm_model)

#e
library(ggplot2)
ggplot(cex5_small, aes(x=income, y=ln_foodaway))+
  geom_point(color="blue",alpha=0.8,pch=16,size=2)+
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.2) +
  labs(x = "INCOME (in $100 units)",
       y = "ln(FOODAWAY)") +
  theme_minimal()
#f
library(ggplot2)

cex5_small$ln_foodaway <- ifelse(cex5_small$foodaway > 0, log(cex5_small$foodaway), NA)
cex5_filtered <- na.omit(cex5_small[, c("ln_foodaway", "income")])

lm_model <- lm(ln_foodaway ~ income, data = cex5_filtered)

cex5_filtered$residuals <- residuals(lm_model)

ggplot(cex5_filtered, aes(x = income, y = residuals)) +
  geom_point(color = "blue", pch = 16, alpha = 0.6) +  
  geom_hline(yintercept = 0, color = "red", linewidth = 1) + 
  labs(title = "Residuals vs. INCOME",
       x = "INCOME (in $100 units)",
       y = "Residuals") +
  theme_minimal()
