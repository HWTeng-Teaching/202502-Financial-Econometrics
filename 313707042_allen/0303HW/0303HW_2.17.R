library(POE5Rdata)
data("collegetown")
?collegetown
#a
plot(collegetown$sqft, collegetown$price,
xlab="house size(SQFT)",
ylab="price(in thouands of dollars)",
pch=16, col="blue", cex=0.6)

#b
lm_model<-lm(price~sqft,data=collegetown)
summary(lm_model)
abline(lm_model,col="red",lwd=2)

#c
lm_quad<-lm(price~I(sqft^2),data=collegetown)
summary(lm_quad)

#d
library(ggplot2)

beta1 <- coef(lm_quad)[1]  
beta2 <- coef(lm_quad)[2] 


sqft_vals <- seq(min(collegetown$sqft), max(collegetown$sqft), length.out = 500)
price_preds <- beta1 + beta2 * sqft_vals^2 

sqft_20 <- 20
price_2000 <- beta1 + beta2 * sqft_20^2


ME <- 2 * beta2 * sqft_20

tangent_sqft <- seq(sqft_20 - 10, sqft_20 + 10, length.out = 10)
tangentb_price <- price_2000 + ME * (tangent_sqft - sqft_20)

ggplot()+
  geom_point(data = collegetown, aes(x = sqft, y = price), color = "blue", alpha = 0.5) +
  geom_line(aes(x = sqft_vals, y = price_preds), color = "red", linewidth = 1.2) +
  geom_line(aes(x = tangent_sqft, y = tangent_price), color = "black", linetype = "dashed", linewidth = 1) +
  geom_point(aes(x = sqft_20, y = price_2000), color = "black", size = 3) +
  
  labs(x = "house size(hundreds of square feet)",
       y = "price(in thouands of dollars)") +
  
  theme_minimal()

#f
#(b)
residuals_linear<-residuals(lm_model)
#(c)
residuals_quad<-residuals(lm_quad)

library(ggplot2)
#(b)
ggplot(data=collegetown,aes(x=sqft,y=residuals_linear))+
  geom_point(color="blue")+
  geom_hline(yintercept=0,color="red")+
  labs(x="SQFT",
       y ="RESIDUALS") +
theme_minimal()
#(c)
ggplot(data=collegetown,aes(x=sqft,y=residuals_quad))+
  geom_point(color="blue")+
  geom_hline(yintercept=0,color="red")+
    labs(x="SQFT",
         y="RESIDUALS")+
theme_minimal()

#g
SSE_linear<-sum(residuals_linear^2)
SSE_quad<-sum(residuals_quad^2)

print(SSE_linear)
print(SSE_quad)



