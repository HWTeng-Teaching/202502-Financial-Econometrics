# 3.7
# b


BACHELOR <- seq(0, 51, by=1) 
a <- 11.5163  
beta <- 1.029 


INCOME <- a + beta * BACHELOR


plot(BACHELOR, INCOME, type="l", col="blue", lwd=2, 
     xlab="Percentage of Population with Bachelor's Degree", 
     ylab="Income per Capita (Thousands of Dollars)", 
     main="Estimated Relationship Between Income and Education")
grid()

