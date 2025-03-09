# 2.17
## (a) (b)

![image](https://github.com/user-attachments/assets/d546591e-87df-4a7d-9982-56abddab735b)
B1=-115.42361, B2=13.40294，B1表當SOFT為0時的房價(實際上沒有SOFT為0的狀況)，B2表示屋面積每增加 100 平方英尺，房價平均變動的金額。
## (c) (d) (e)
![image](https://github.com/user-attachments/assets/75272577-7c7d-410c-b830-9ee56f4a0cca)
Marginal Effect at 2000 sqft: 6.4480916518131
Elasticity at 2000 sqft: 0.7565249452144
## (f) (g)
![image](https://github.com/user-attachments/assets/0d761be9-42f7-4e59-b23e-d920975366d5)
根據殘差圖，殘差明顯在房屋大時較房屋小還要大，可能不滿足同質變異數假設  
SSE for Linear Model: 5262846.94710885, SSE for Quadratic Model: 4207791.38876957  
Quadratic Model的SSE較小，因為 Quadratic Model捕捉到更多房價的變異性，也就是房價更能夠以Quadratic Model解釋，代表Quadratic Model為 “better-fitting” model。
# 2.25
## (a)
![image](https://github.com/user-attachments/assets/cc230d98-f5ba-46af-9c32-8c8067665864)  

Mean of foodaway: 49.27085  
Median of foodaway: 32.555  
25th Percentile: 12.04  
75th Percentile: 67.5025  
## (b)
Mean foodaway (Advanced Degree): 73.1549416342412  
Median foodaway (Advanced Degree): 48.15  
Mean foodaway (College Degree only): 48.5971815718157  
Median foodaway (College Degree only): 36.11  
Mean foodaway (No College/Advanced Degree): 39.010174216027 
Median foodaway (No College/Advanced Degree): 26.0
## (c)
![image](https://github.com/user-attachments/assets/199be332-afaf-48bd-a211-29227ffe2017)  

Mean of ln(foodaway): 3.65080360893854  
Median of ln(foodaway): 3.68649879556308  
25th Percentile of ln(foodaway): 3.07592881554827  
75th Percentile of ln(foodaway): 4.27971701584931  
當觀察值為0時，無法取對數，因此取對數後的觀察值會比較少

## (d)
![image](https://github.com/user-attachments/assets/c64c052a-a232-45f5-a60a-61034b6c37e6)  
根據回歸結果，當家庭月收入增加 100 美元，則外食支出 (foodaway) 平均增加約 0.69%
## (e)
![image](https://github.com/user-attachments/assets/87fa2e15-ce1d-4423-bd57-33f1aa682756)
## (f)
![image](https://github.com/user-attachments/assets/ce247092-1574-467d-bb3a-a2c3ca70f0f1)  
看起來殘差為隨機，同質變異數假設為合理假設
# 2.28
# (a)
![image](https://github.com/user-attachments/assets/62e6a6fc-59b3-4d25-8db9-5ac4cbc8edc5)
![image](https://github.com/user-attachments/assets/f649f0b2-7428-444a-bd45-0e3674a280ca)  
薪資的部分可以看出明顯的右尾分布，且具有極端的離群值。教育程度呈現左尾分布，大部分的數據集中在10年~15年之間

# (b)
![image](https://github.com/user-attachments/assets/b459af70-aa3a-412d-9ca5-967518e67856)  
根據結果，回歸截距及斜率皆在統計上顯著。教育每增加一年，平均每小時薪資增加2.3968元
# (c)
![image](https://github.com/user-attachments/assets/572083fb-bc04-487f-b9d9-266f25255900)  
可以看出當教育年限增加，薪資的變異程度會變大。若SR1~SR5的假設成立，殘差圖不應該呈現特定的模式，應該呈現隨機的狀態。
# (d)









