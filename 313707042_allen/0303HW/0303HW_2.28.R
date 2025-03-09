library(POE5Rdata)
data("cps5_small")
?cps5_small

#a
library(ggplot2)
ggplot(cps5_small, aes(x=wage))+
  geom_histogram(binwidth = 2,fill="blue",color="black",alpha=0.7)+
  labs(x="WAGE(per hour,$)",
     y="Frequency")+
theme_minimal()
summary(cps5_small$wage)

library(ggplot2)
ggplot(cps5_small, aes(x=educ))+
  geom_histogram(binwidth = 2,fill="blue",color="black",alpha=0.7)+
  labs(x="EDUC(years)",
       y="Frequency")+
theme_minimal()
summary(cps5_small$educ)

#b
lm_model<-lm(wage~educ,data=cps5_small)
summary(lm_model)

#c
residuals<-residuals(lm_model)
ggplot(data=cps5_small,aes(x=educ,y=residuals))+
  geom_point(color="blue")+
  geom_hline(yintercept=0,color="red")+
  labs(x="EDUC",
       y ="RESIDUALS") +
  theme_minimal()
#d
data_male<-subset(cps5_small,female==0)
data_female<-subset(cps5_small,female==1)
data_white<-subset(cps5_small,black==0)
data_black<-subset(cps5_small,black==1)

model_male<-lm(wage~educ,data=data_male)
model_female<-lm(wage~educ,data=data_female)
model_white<-lm(wage~educ,data=data_white)
model_black<-lm(wage~educ,data=data_black)

summary(model_male)
summary(model_female)
summary(model_white)
summary(model_black)

#e
lm_quad<-lm(wage~I(educ^2),data=cps5_small)
summary(lm_quad)

alpha_2<-coef(lm_quad)["I(educ^2)"]

marginal_effect_12<-2*alpha_2*12
marginal_effect_16<-2*alpha_2*16

print(marginal_effect_12)
print(marginal_effect_16)

#f
linear_model <- lm(wage ~ educ, data = cps5_small)
cps5_small$educ2 <- cps5_small$educ^2
quad_model <- lm(wage ~ educ + educ2, data = cps5_small)
cps5_small$fitted_linear <- fitted(linear_model)
cps5_small$fitted_quad <- fitted(quad_model)

library(ggplot2)
ggplot(cps5_small, aes(x=educ, y=wage))+
  geom_point(alpha=0.5,color="blue")+
  geom_line(aes(y = fitted_linear), color = "red", size = 1, linetype = "dashed") +
  geom_line(aes(y = fitted_quad), color = "black", size = 1) +
  theme_minimal() +
  labs(
    title = "Linear vs Quadratic Regression",
    x = "Education (Years)",
    y = "Wage"
  ) +
  theme(plot.title = element_text(size = 14, face = "bold"))


