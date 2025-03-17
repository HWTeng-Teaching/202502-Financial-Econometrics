# 3.1
![image](https://github.com/user-attachments/assets/4f7a52d3-774e-44f1-8d5f-28c9a5ee8f18)
## (a) 設定虛無假設(_H<sub>0</sub>_)和對立假設(_H<sub>1</sub>_)
- 虛無假設(_H<sub>0</sub>_)：GDP和奧運獎牌數無關，即 H<sub>0</sub>：β₂ = 0  
- 對立假設(_H<sub>1</sub>_)：GDP和奧運獎牌數有正向關係，即H<sub>1</sub>：β₂ > 0  
- 說明：右尾檢定（right-tailed test），因為假設 GDP 增加會導致獎牌數增加
## (b) 計算檢定統計量（t-statistic）並確定其分佈
- Formula：t = (b₂ − 0) / se(b₂)  
→ t = 0.01309 / 0.00215 ≈ 6.086  
- 分佈：如果虛無假設成立(即β₂ = 0)，自由度(df) = n - 2 = 64 - 2 = 62    
## (c) 若對立假設為真，t 統計量的分佈如何變化
- 若H<sub>0</sub>為真(即β₂ = 0)，則 t 統計量應該遵循標準的 t 分佈，平均值為 0  
- 若H<sub>1</sub>為真(即β₂ > 0)，則 t 統計量的值將變大（正向移動），因為b₂將會大於 0，導致 t 值增加，使其偏離 t 分佈的中心(向右移動)  
## (d) 1% 顯著性水準下，拒絕H<sub>0</sub>的t值臨界點(critical value)
- 顯著水準 𝛼 = 0.01(單尾檢定)，自由度(df) = 62，t<sub>62,0.99</sub> = 2.39  
- 拒絕域(reject region) = t > 2.39
- 若t值落在此區間，則拒絕H<sub>0</sub>，反之則不拒絕H<sub>0</sub>
## (e) 進行 t 檢定/說明經濟含義與顯著水準(1%)
- t = 6.086 > 臨界點(critical value) = 2.39；拒絕虛無假設(H<sub>0</sub>)
- 經濟含義：GDP 較高的國家往往能贏得較多的獎牌，可能是因為這些國家擁有更多的運動資源、良好的基礎設施、更完善的運動訓練體系等
- 顯著水準(1%)：拒絕虛無解假設(H<sub>0</sub>)的錯誤率只有 1%  
## *R code*
<img width="356" alt="image" src="https://github.com/user-attachments/assets/0fe72546-ef36-48ce-aa2f-d4829a67959b" />

---
# 3.7
![image](https://github.com/user-attachments/assets/ec549157-f45c-4138-a631-4a597a546c92)
## (a)


## (b)
## (c)
## (d)
## (e)
## (f)
## (g)
---
# 3.17
![image](https://github.com/user-attachments/assets/95bb8fb4-12ff-40a9-984b-96a75a51008f)
## (a)
### 設定虛無假設(_H<sub>0</sub>_)和對立假設(_H<sub>1</sub>_)
- 虛無假設(_H<sub>0</sub>_)：β₂ = 1.8
- 對立假設(_H<sub>1</sub>_)：β₂ > 1.8
### 計算檢定統計量(t-value)
- t = (b₂ − 0) / se(b₂) = (2.46 − 1.80) / 0.16 ≈ 4.125
### 查找 t 臨界值(critical value)
- 顯著水準 𝛼 = 0.05(單尾檢定)，自由度(df) = 986 - 2 = 984
- t<sub>984,0.95</sub> = 1.645
### 決策
- 若𝑡 > 1.645，則拒絕
- t = 4.125 >> crtical value = 1.645，拒絕虛無假設(_H<sub>0</sub>_)
### 結論
- 在95%信心水準下，urban regression 斜率將顯著大於1.8
## (b)
- Rural點估計(EDUC=16)：WAGE = −4.88 + 1.80(16) = −4.88 + 28.8 = 23.92
- 標準誤 = 0.833 (題目給訂)
- 自由度(df) = 214 - 2 = 212
- t<sub>0.975,212</sub> ≈ 1.97
- 信賴區間(CI) = 23.92 ± 1.97 × 0.833 → CI=(22.28,25.56)

## (c)
## (d)
---
# 3.19
![image](https://github.com/user-attachments/assets/68121018-af9b-484e-a7d7-558db45047b2)
## (a)
## (b)
## (c)
## (d)
## (e)
