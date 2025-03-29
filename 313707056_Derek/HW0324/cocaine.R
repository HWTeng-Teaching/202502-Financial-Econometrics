library(POE5Rdata)
data("cocaine")
model<-lm(price~.,data=cocaine)
summary(model)



