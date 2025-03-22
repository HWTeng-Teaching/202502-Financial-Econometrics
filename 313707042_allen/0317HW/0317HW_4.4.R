library(POE5Rdata)
#a
intercept<-64.289
slope<-0.990

EXPER<-seq(0,30)
RATING<-intercept+slope*EXPER

plot(EXPER,RATING,type="l",col="blue",lwd=2,
     xlab="EXPER",ylab="RATING")
grid()

#b
intercept<-39.464
slope<-15.312

EXPER<-seq(1,30)
RATING<-intercept+slope*log(EXPER)

plot(EXPER,RATING,type="l",col="blue",lwd="2",
     xlab="EXPER",ylab="RATING")
grid()

