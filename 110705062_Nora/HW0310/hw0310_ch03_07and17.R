##3.7.5
df <- 49  
t_stat <- 0.567  
alpha <- 0.05 
t_critical <- qt(1 - alpha / 2, df) 

x <- seq(-4, 4, length=1000)
y <- dt(x, df)

plot(x, y, type="l", lwd=2, col="lightblue",
     xlab="t-value", ylab="Density",
     main="t-Distribution with p-Value and Rejection Regions")

polygon(c(x[x > t_stat], rev(x[x > t_stat])), 
        c(y[x > t_stat], rep(0, sum(x > t_stat))), 
        col="pink", border=NA)
polygon(c(x[x < -t_stat], rev(x[x < -t_stat])), 
        c(y[x < -t_stat], rep(0, sum(x < -t_stat))), 
        col="pink", border=NA)

polygon(c(x[x > t_critical], rev(x[x > t_critical])), 
        c(y[x > t_critical], rep(0, sum(x > t_critical))), 
        col="lightblue", border=NA)
polygon(c(x[x < -t_critical], rev(x[x < -t_critical])), 
        c(y[x < -t_critical], rep(0, sum(x < -t_critical))), 
        col="lightblue", border=NA)

abline(v = c(-t_stat, t_stat), col="pink", lty=2, lwd=2)
abline(v = c(-t_critical, t_critical), col="lightblue", lty=2, lwd=2)

##3.17.1
df <- 986 - 2  
t_stat <- 4.125 
t_critical <- qt(0.95, df)
x <- seq(-4, 6, length=1000)
y <- dt(x, df)
df <- 986 - 2
t_stat <- 4.125 
t_critical <- qt(0.95, df)  
x <- seq(-4, 6, length=1000)
y <- dt(x, df)

plot(x, y, type="l", lwd=2, col="blue", 
     xlab="t-value", ylab="Density",
     main="t-Distribution with Critical Region")
polygon(c(x[x > t_critical], rev(x[x > t_critical])), 
        c(y[x > t_critical], rep(0, sum(x > t_critical))), 
        col=rgb(1, 0, 0, 0.5), border=NA)

abline(v = t_stat, col="black", lwd=2, lty=2) 
abline(v = t_critical, col="red", lwd=2, lty=3) 


