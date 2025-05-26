# 11.28
![image](https://github.com/user-attachments/assets/3a605b61-eceb-44fb-8483-54eda36862a6)

## (a) 需求與供給方程式的推導重整

**需求方程式的重整：**

$$
Q = \alpha_1 + \alpha_2 P + \alpha_3 PS + \alpha_4 DI + e^d
$$

改寫為：

$$
P = \frac{1}{\alpha_2} \left(Q - \alpha_1 - \alpha_3 PS - \alpha_4 DI - e^d\right)
$$

令：

$$
P = \delta_1 + \delta_2 Q + \delta_3 PS + \delta_4 DI + u^d
$$

**經濟理論解釋**：

- δ_2 < 0 ：價格與需求量呈負相關（反向關係）。
- δ_3 > 0 ：替代品價格上升，會使松露需求增加，導致價格上升。
- δ_4 > 0 ：如果可支配所得上升，且松露為正常財，則需求增加，價格上升。

---

**供給方程式的重整：**

$$
Q = \beta_1 + \beta_2 P + \beta_3 PF + e^s
$$

改寫為：

$$
P = \frac{1}{\beta_2} \left(Q - \beta_1 - \beta_3 PF - e^s\right)
$$

令：

$$
P = \phi_1 + \phi_2 Q + \phi_3 PF + u^s
$$

**經濟理論解釋**：

- Φ_2 > 0 ：價格與供給量呈正相關（正向關係）。
- Φ_3 > 0 ：生產要素價格上升會降低供給，進而推升均衡價格。

## (b) 二階段最小平方法（2SLS）估計結果

<img width="555" alt="OLS 2SLS" src="https://github.com/user-attachments/assets/9f0db65e-a243-4387-acf2-46bf13b4dfbe" />


**註解**：
- 標準誤差以括號表示。
- 顯著性符號：  
  - \* 表示 \(p < 0.05\)  
  - \*\* 表示 \(p < 0.01\)  
  - \*\*\* 表示 \(p < 0.001\)

**結論**
> 符號方向與 (a) 小題的預期一致，且所有係數皆與 0 有顯著差異，因為其 p 值皆小於 0.05 顯著水準。

## (c) 平均值下的需求價格彈性（Price Elasticity of Demand)

<img width="762" alt="C" src="https://github.com/user-attachments/assets/d5396a43-9860-4220-902d-ffb2c44dea86" />


需求價格彈性在平均值處的計算公式為：

$$
\varepsilon_D = \frac{\%\Delta Q}{\%\Delta P} = \frac{\Delta Q / \overline{Q}}{\Delta P / \overline{P}} = \frac{1}{\delta_2} \times \frac{\overline{P}}{\overline{Q}}
$$

套用估計值：

$$
\hat{\varepsilon}_D = \frac{1}{\delta_2} \times \frac{\overline{P}}{\overline{Q}} 
= \frac{1}{-2.6705} \times \frac{62.724}{18.458} 
= -1.2725
$$

- δ_2  為價格變數在需求函數中的係數（理論上應為負值）。
- P^- 為價格的樣本平均數。
- Q^- 為數量的樣本平均數。
- 結果 \( \hat{\varepsilon}_D = -1.2725 \) 表示：在平均值處，價格每變動 1%，需求量約變動 1.27%，彈性大於 1，屬於**有彈性需求**。


## (d)


<img width="699" alt="d1" src="https://github.com/user-attachments/assets/364f22a9-9448-4ef4-9224-94bfafd25c4e" />




## (e) 均衡價格與數量的計算

<img width="861" alt="e" src="https://github.com/user-attachments/assets/b37a4775-6988-468c-a5f6-956ad55e7faf" />



根據 (d) 小題所建立的供需模型，將外生變數的估計值代入後，令供需相等可得：

$$
111.5801 - 2.6705 Q_{EQM} = 9.2470 + 2.9367 Q_{EQM}
$$

解得：

$$
Q_{EQM} = 18.2503
$$

將 \( Q_{EQM} \) 代入需求方程式中求得均衡價格：

$$
P_{EQM} = 111.5801 - 2.6705 \times 18.2503 = 62.8427
$$

使用簡約式（Reduced Form）估計值求解的均衡值：

由表格 11.2a 與 11.2b 可得：

$$
Q_{EQM\_RF} = 7.8951 + 0.6564 \times 22 + 2.1672 \times 3.5 - 0.5070 \times 23 = 18.2604
$$

$$
P_{EQM\_RF} = -32.5124 + 1.7081 \times 22 + 7.6025 \times 3.5 + 1.3539 \times 23 = 62.8154
$$


比較由結構模型（part d）計算的均衡值與簡約模型（reduced form）估計值，可以發現結果幾乎一致，表示模型推估具備良好一致性。

- 結構模型得：
  - \( Q_{EQM} = 18.2503 \)
  - \( P_{EQM} = 62.8427 \)

- 簡約模型得：
  - \( Q_{EQM\_RF} = 18.2604 \)
  - \( P_{EQM\_RF} = 62.8154 \)

兩組結果差距極小，模型合理。


## (f) OLS 與 2SLS 模型估計結果的比較與解釋

請使用普通最小平方法（OLS）估計 (a) 中所建立的需求與供給方程式。  請檢查估計係數的符號是否正確？估計結果是否顯著不為 0？ 並與 (b) 小題中的結果進行比較。

需求方程式的 OLS 估計結果見表 XR 11.28 的第 (2) 欄，供給方程式的 OLS 結果亦在第 (2) 欄中。 除了截距項與 \( Q \) 的係數之外，其餘係數皆顯著不為零。  

然而，\( Q \) 的係數符號是錯誤的，因為它暗示價格與需求量之間存在正相關， 這與經濟理論（需求應為負斜率）相矛盾。 與 (b) 小題的 2SLS 結果比較，\( Q \) 的係數符號相反，而且估計的截距項與 \( PS \) 的係數值也都小得多。供給方程式的所有估計係數在統計上皆顯著不為零，其符號正確，且係數大小與 (b) 小題的估計值相近。


<img width="745" alt="f" src="https://github.com/user-attachments/assets/6459486a-2bbc-4429-a398-67e8754d73b8" />

------
# 11.30
![image](https://github.com/user-attachments/assets/f0d2a691-7fc5-4910-926d-581d7d9ac53a)

## (a) 投資函數的估計與檢定結果

OLS 對投資函數的估計結果見表 XR 11.30 的第 (1) 欄。自由度為 17，t 分配在 97.5% 百分位的臨界值為 2.1098，95% 百分位為 1.7396。  
從表中可見利潤 (P) 與滯後利潤 (PLAG) 在 5% 顯著水準下具有預期的正號。若利潤較高，企業會增加投資支出。  
至於滯後資本存量 (KLAG) 的符號為負，且顯著，說明資本存量越大，對新投資的需求越小。


  Table:

<img width="682" alt="table" src="https://github.com/user-attachments/assets/f4a6ddb8-4d9f-4fea-a496-7db21a4dfe75" />

註解與顯著性說明：

-  P ：當期利潤，理論上應該正向影響投資。
-  PLAG ：滯後一期的利潤，同樣預期為正向影響。
-  KLAG ：滯後資本存量，為負值，代表資本基數越大，新投資需求越少。
-  C ：常數項，各模型變動大，但在 2SLS 與 Hausman 檢定中高度顯著。
- 模型 Sargan 中的 VHAT 為誤差項工具變數的預測殘差，顯著，表示存在工具變數相關性（有效性檢定）。

顯著性標註依據：
- \*：\( p < 0.05 \)
- \*\*：\( p < 0.01 \)
- \*\*\*：\( p < 0.001 \)

## (b) F 檢定與虛無假設檢定結果


本檢定針對 Table 第 (4) 欄中的 5 個變數之係數是否同時為 0 進行 F 檢定。

- F 統計量為  1.93 
- 5% 顯著水準下，臨界值為  F_{(0.95, 5, 13)} = 3.0254 

由於 ( 1.93 < 3.0254 )，因此**無法拒絕虛無假設**，  


即認為這 5 個變數的係數在統計上並不顯著不同於零。


## (c) Hausman 檢定與內生性判斷

Hausman 檢定的回歸結果位於表 XR 11.30 的第 (5) 欄。  

注意：殘差項 VHAT 在顯著水準 **0.001** 下依然顯著，表示其具有統計意義。

因此我們得出結論：  
-  P  是**內生變數**（endogenous）

這與同時方程模型（simultaneous equations model）中對  P  為內生變數的預期一致。


## (d) 比較 2SLS 與 OLS 的估計差異

根據表 XR 11.30 第 (2) 欄（2SLS 結果），觀察到以下明顯差異：

- 利潤變數  P  在 2SLS 模型中估計結果**不顯著**。
- 滯後利潤  PLAG  的係數約為 OLS 估計值的兩倍，且在 **1% 顯著水準**下達到統計顯著。
- 滯後資本存量  KLAG  的係數為負且在 **1% 顯著水準**下顯著，這與 OLS 模型結果一致。


## (e) 第二階段模型中的標準誤差與殘差計算

第二階段模型結果列於 Table XR 11.30 的第 (3) 欄中。雖然係數估計值一致，但 **t 值不同**，  
原因是錯誤地使用了不正確的 \( \text{var}(e_{2t}) \) 估計，導致標準誤差計算有誤。

錯誤的殘差計算方式（用於估計變異數）：

<img width="227" alt="1" src="https://github.com/user-attachments/assets/75b3f6ab-bc31-4f22-a3a8-ee73ecb1831f" />


正確的殘差應為：

<img width="227" alt="2" src="https://github.com/user-attachments/assets/f7f83c6a-175e-407d-b095-2acf29a8a4b1" />

- 錯誤版本使用了預測值 \( \hat{P}_t \)（即第一階段的預測利潤）作為解釋變數進行殘差估計，
  這會低估標準誤差，導致 t 統計量誤差。
- 正確版本應使用實際觀察值 \( P_t \) 來估算誤差項，才能正確估計變異數與 t 值。

## (f) Sargan 檢定：過度識別工具變數的有效性檢定

Sargan 檢定結果見於 Table XR 11.30 的第 (6) 欄。  
檢定統計量計算如下：

$$
TR^2 = 21 \times 0.0864 = 1.815
$$

在自由度為 4 的卡方分配下，5% 顯著水準下的臨界值為：

$$
\chi^2_{(0.05, 4)} = 9.4877
$$


由於檢定統計量 \( TR^2 = 1.815 \lt 9.4877 \)，**無法拒絕虛無假設**，  
即：我們**無法拒絕多餘工具變數（surplus IV）為有效的假設**。




> Cannot reject the validity of the surplus instruments; over-identifying restrictions hold.
