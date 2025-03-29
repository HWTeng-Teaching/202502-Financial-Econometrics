## ch5 Q23

rm(list=ls())  
library(POE5Rdata)  
data("cocaine") 


# 進行回歸分析
model <- lm(price ~ quant + qual + trend, data = cocaine)

# 顯示回歸結果
summary(model)


![image](https://github.com/user-attachments/assets/452a684f-f3b6-44a7-aa89-e3f9ddb7b909)
