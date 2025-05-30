# 15.6
![image](https://github.com/user-attachments/assets/daf8aa23-e054-4223-bfc7-022cbacbc540)
ANS:   

資料：NLS（National Longitudinal Survey）年輕女性樣本

本題僅取 1987 與 1988 兩年：

𝑁=716 人 ⇒ 橫斷面 716 筆，面板共 2×716=1432 筆。

變數

lnWAGE ：工資對數

EXPER  ：工作年資

EXPER2 ：工作年資^2

SOUTH ：南方居住虛擬變數（1=住南方，0=其他）

UNION ：是否為工會會員（1=是，0=否）


**(a)1987 與 1988 兩年度的 OLS──異質性假設為何？**

<img width="564" alt="a" src="https://github.com/user-attachments/assets/2d071013-9dac-45ca-935c-2766fb12a0e7" />

係數在兩年 非常接近；使用單一年份迴歸，等於 假設每位受訪者的截距與斜率都相同，僅允許截距隨年份移動，這個基本假設意謂著不同個體之間不存在異質性（heterogeneity）。（即假設 𝑢𝑖=0)

**(b)指定為 Panel Random-Effects (XR15.6)**

<img width="584" alt="b" src="https://github.com/user-attachments/assets/6426f2ee-0964-4f85-afc1-2e6806521b25" />

與 (a) 比較：此處 允許每一位女性有自己的隨機截距𝑢𝑖，但仍假設斜率在個體之間相同。

模型關鍵概念：

𝑢𝑖: 可視為個體的「先天差異」，如公司規模、地理位置、教育背景等，這些差異在時間內不變但在個體間不同。

𝜀𝑖𝑡: 則是「偶發性波動」，如每年受到的景氣循環、操作失誤等，會隨時間與個體變動。

面板模型的意義：這種模型結構的設定可以幫助我們區分出：

橫斷面異質性（cross-sectional heterogeneity）→ 𝑢𝑖

時間序列誤差成分（serial randomness）→ 𝜀𝑖𝑡

​RE 估計結果（表 15.10 第 5 欄）𝛽^2=0.0986,SE=0.0220 ，95 % CI ﹛0.0556,0.1415｝→ 與 (a) 相比係數更小、標準誤更低（效率較高）。

**(c)固定效果 (FE) 與 OLS 的差異──哪個係數變化最大？**

所有固定效果模型（Fixed Effects）下的係數估計值看起來都顯著不同。我們利用固定效果模型構造出粗略的 95% 信賴區間後，得到以下變數的估計區間：

其中，OLS 模型中 EXPER 的係數估計值不在固定效果模型的信賴區間內，這表示這兩種模型的估計結果在此變數上有「統計上顯著的差異」。但其他變數的 OLS 估計值都落在其對應的固定效果信賴區間內，代表在這些變數上，兩種模型的估計結果沒有顯著差異。

固定效果估計下的 95% 信賴區間（CI）

| 變數      | 固定效果估計的 95% 信賴區間         | OLS 估計值是否落在 CI 中 | 解釋                          |
|-----------|--------------------------------------|----------------------------|-------------------------------|
| `EXPER`   | (-0.0085, 0.1235)                    | ❌ 否                       | OLS 與 FE 有統計顯著差異     |
| `EXPER2`  | (-0.0034, 0.0010)                    | ✅ 是                       | 無顯著差異                    |
| `SOUTH`   | (-0.5777, -0.0745)                   | ✅ 是                       | 無顯著差異                    |
| `UNION`   | (0.0198, 0.1446)                     | ✅ 是                       | 無顯著差異                    |

##

模型比較重點

- **OLS 模型假設**：所有個體的截距與斜率完全相同，忽略個體間異質性。
- **固定效果模型（FE）**：允許每個個體有自己的截距（即控制 u_i ）。
- 如果某變數在 OLS 與 FE 的估計差異大，可能代表 OLS 存在偏誤，應採用 FE 模型。

結論

- `EXPER` 的估計在兩模型間具有統計顯著差異，建議使用 Fixed Effects 來控制個體異質性。
- 其他變數雖有數值差異，但不具統計顯著性，推論 OLS 仍能提供合理估計。

**(d)檢定固定效果是否存在（個體異質性）**

自由度計算

- 分子自由度（個體數 − 1）： df_1 = N - 1 = 716 - 1 = 715
- 分母自由度（總觀察數 − 個體數 − 參數數）：df_2 = NT - N - K = 1432 - 716 - 4 = 712  
- 模型參數個數： K = 5

F 統計量與臨界值

- 計算所得 F 值： F = 11.68
- 1% 臨界值（查表）：
  
  $$
  F_{crit} = 1.0 \text{（Statistical Table 5）},\quad 1.1904959 \text{（R Code）}
  $$

結論

因為：F = 11.68 > F_{crit} = 1.1904959

所以我們 **拒絕虛無假設 H_0 **，表示存在顯著的個體異質性。

✅ 建議使用 **固定效果模型（Fixed Effects Model）**，以控制個體間差異。


**(e)組內轉換與 Cluster-Robust 標準誤解釋**

組內轉換後誤差項：

<img width="108" alt="e" src="https://github.com/user-attachments/assets/a0e50a05-4c15-49d2-b8f3-29afe180a62f" />


即使原始 \( e_{it} \) 之間無相關性，轉換後的 \( \tilde{e}_{it} \) 仍會出現時間序列上的 **自我相關（serial correlation）**。


為何使用 Cluster-Robust 標準誤？

- 考慮以下兩種誤差結構：
  - 異質變異數（heteroskedasticity） across individuals/time
  - 誤差的自相關（serial correlation）

- Cluster-Robust SE 修正這些問題，使估計更穩健，避免標準誤低估導致的假顯著。

 傳統 vs. Robust 標準誤比值分析

| 變數      | 傳統 / Robust SE 比值 | 詮釋                            |
|-----------|------------------------|---------------------------------|
| `EXPER`   | 1.0061                 | 幾乎一致，估計穩健               |
| `EXPER2`  | 1.0000                 | 完全一致                         |
| `SOUTH`   | 0.5042                 | Robust SE 為傳統值的約 2 倍       |
| `UNION`   | 0.8501                 | Robust SE 為傳統值的約 1.18 倍    |

 結論

- 若誤差具有異質性或時間自相關，建議使用 **Cluster-Robust 標準誤**。
- 可以有效避免假設檢定失真，提高模型推論的可靠性。


**(f)　RE vs. FE 的 Hausman 檢定**

估計比例（Random Effects / Fixed Effects）

| 變數      | RE / FE 比例 |
|-----------|--------------|
| `EXPER`   | 1.71         |
| `EXPER2`  | 1.92         |
| `SOUTH`   | 0.71         |
| `UNION`   | 1.25         |

Hausman 檢定統計量（t 值）

| 變數      | t 統計量 | 顯著性 | 說明                         |
|-----------|----------|--------|------------------------------|
| `EXPER`   | -1.67    | 10%    | 有潛在內生性                |
| `EXPER2`  | 1.29     | 否     | 無顯著差異                  |
| `SOUTH`   | -0.77    | 否     | 同上                        |
| `UNION`   | -1.06    | 否     | 同上                        |

結論

- 僅 `EXPER` 顯示 RE 與 FE 有顯著差異（10% 水準）
- **整體上支持使用隨機效果模型（RE）**
- 使用傳統 FE 標準誤進行 Hausman 檢定，以符合古典假設（誤差項同質且無自相關）
- **Stata 14.2 不允許在 robust 標準誤下進行 Hausman 檢定**

------
# 15.17
![image](https://github.com/user-attachments/assets/04714aaf-d16b-41f6-bc95-42379afe9d67)

ANS：   

**（a）以一階差分資料估計的迴歸式為**

1. **產生差分變數**

<img width="606" alt="a" src="https://github.com/user-attachments/assets/370525f1-f615-4626-8309-9c736ff640b6" />


2. **回歸（無截距）**

<img width="226" alt="a1" src="https://github.com/user-attachments/assets/670cb521-dca5-4c68-abba-e330978071ec" />


3. **估計結果**

$$
\hat{\beta}_{2}=0.02975,\qquad \text{SE}=0.02922.
$$

4. **95 % 信賴區間**

$$
[-0.02841,\;0.08791].
$$

> 區間包含 0，無法拒絕「所得差分不影響酒類支出差分」的假說。


**（b）隨機效果（RE）模型**

<img width="589" alt="b r" src="https://github.com/user-attachments/assets/22b3d2df-fb86-4488-9569-e81fe7d48622" />


##
<img width="516" alt="b" src="https://github.com/user-attachments/assets/6529a5fb-83d2-446a-a5f2-a1e487e78031" />

##

* 95 % 信賴區間   [0.01283 , 0.04032].

係數顯著為正，且區間明顯 **較 (a) 窄**（標準誤約為差分估計的 1/4），顯示利用戶間變異帶來效率增益。

亦即在 95% 信心水準下，家庭每增加 1,000 美元所得，酒類支出將增加 12.83 到 40.32 美元。隨機效果估計的係數略小於差分估計，但其標準誤僅為差分估計的約 25%，故具統計顯著性。


**(c）LM 檢定（Breusch–Pagan)** 

<img width="504" alt="c r" src="https://github.com/user-attachments/assets/a12e279b-eae4-4a02-b791-a20108ebd5df" />

##

$$
\text{LM}= 
\sqrt{\frac{NT}{2(T-1)}}\!\left(\;
\frac{\displaystyle \sum_{i=1}^{N}\bigl(\sum_{t=1}^{T}\hat e_{it}\bigr)^{2}}
     {\displaystyle \sum_{i=1}^{N}\sum_{t=1}^{T}\hat e_{it}^{\,2}}
-1\right).
$$

<img width="437" alt="c" src="https://github.com/user-attachments/assets/8a489094-d997-48c3-b791-f0dcd90a0d79" />

##

若無未觀察個體異質性，LM 服從標準常態分配，其 5% 臨界值為 ±1.96。由於 4.5475 超過臨界值，拒絕虛無假設 

𝜎𝑢2=0，接受 𝜎𝑢2>0σ 。表示存在統計上顯著的未觀察異質性。（部分軟體回報 LM² = 20.68；對應的 𝜒0.95,12=3.841，結論相同。）


**（d）Mundlak 檢定（控制戶別平均）**

<img width="577" alt="d r" src="https://github.com/user-attachments/assets/bd45a73d-501e-4ee4-8cca-5e712064df92" />

##

<img width="494" alt="d" src="https://github.com/user-attachments/assets/7f53ce33-54c9-41e2-acdc-1af4067bae8f" />

INCOMEM 係數的 t 值僅 0.30，統計上不顯著。依 Mundlak 檢定，沒有證據顯示所得與未觀察個體效果 𝑢𝑖相關。（雖非必要，Hausman 檢定亦給出統計量 0.09 < 3.841，同樣不拒絕隨機效果的外生性假設。）結論：所得與隨機個體效果不相關，(b) 的隨機效果估計適當且優於差分估計。

------
# 15.20
![image](https://github.com/user-attachments/assets/570e1978-99af-4a2d-bf17-219bcb5bde66)

ANS:   

**Table**

<img width="651" alt="Table" src="https://github.com/user-attachments/assets/0b52c3be-5a86-4a48-87d0-a0f72d9ea195" />


**(a)** OLS 估計與解釋（見 Table  第一欄）

1.小班效果（SMALL）：若學生從一般班轉到小班，平均閱讀成績估計可提高 5.8 分。係數在 1% 顯著水準下顯著不為零。

2.助教效果（AIDE）：相較於一般班，有無全職助教對平均閱讀成績無顯著影響。

3.教師年資（TCHEXPER）：教師每增加 1 年教學經驗，平均閱讀成績估計 提高 0.49 分。係數顯著不為零。

4.性別與族裔：男生的平均閱讀成績估計比 女生低 6 分。白人／亞裔學生的平均閱讀成績估計比 黑人學生高 3.9 分。

5.經濟弱勢（FREELUNCH）：領取免費午餐（低社經地位）的學生，其平均閱讀成績估計比未領取者 低 14.8 分。

除了 AIDE 之外，所有解釋變數的係數在個別檢定下皆具統計顯著性。

**(b)** 學校固定效果 (FE, LSDV) 與 F 檢定（見 Table 第三欄）

整體結論與 OLS 模型相同。具體而言：

1.小班效果（SMALL）：若學生就讀小班，平均閱讀成績估計可提升 6.49 分，比 OLS 的 5.82 分稍高。

2.教師年資（TCHEXPER）：教師每增加 1 年教學經驗，平均閱讀成績的估計增幅降為 0.29 分（低於 OLS 的 0.49 分）。

3.性別與族裔差異：男生與女生的平均閱讀成績差距略小於 OLS 所估。白人／亞裔學生與黑人學生之間的平均成績差距約 8 分，大約是 OLS 估計值的兩倍。

**(c)**固定效果顯著性 F 值（16.70；df = 78, 5681）

<img width="539" alt="cpng" src="https://github.com/user-attachments/assets/b3433ae7-685d-4b85-aa7b-c214addbaa97" />


F 檢定統計量為 16.70。臨界值 

𝐹(0.95,78,5681)=1.2798

​因此我們拒絕「各學校之間沒有顯著差異」的虛無假設。若學校指標變數與模型中已納入的解釋變數不相關，則是否將這些學校指標變數納入迴歸，對估計結果的影響應很有限。

**(d)** 學校隨機效果 (RE) 與 Breusch-Pagan LM 檢定

隨機效果估計結果列於 Table 的第 (2) 欄。整體而言，隨機效果係數與 OLS 估計十分接近，也與固定效果估計相當類似（雖然這兩者尚未進行正式檢定）。

1.潛在與校級效果相關的變數

在 Project STAR 中，學生是在「校內」隨機分派班級，而並非「跨校」隨機分派。部分學校位於富裕學區，部分則位於較貧困的學區。家庭所得會影響孩童的學習環境與資源，而這些差異大致可由 FREELUNCH（免費午餐指標）捕捉。此外，較富裕的學區可能支付更高薪資，因而聘用能力更強、經驗更豐富的教師。這些因素都可能與學校隨機效果相關。

2.LM 檢定：校級隨機效果是否存在

(15.35) 的 LM 檢定在此並不完全適用，因為每所學校的學生人數並非固定。為簡化起見，以「每校平均觀測值」, 𝑇ˉ=73 代入計算。得到的 LM 統計量為 2247.96，遠大於標準常態臨界值 1.96。以 Stata 軟體計算的 LM2 值為 6677.42，亦大於 𝜒2(1)臨界值 3.84。因此，我們認為各校之間確實存在未觀測的異質性。

**(e)**FE vs RE 單變數差異 t-檢定、Hausman 檢定 (χ² = 22.62, df = 6)

對於單一變數𝑘的比較，t 統計量定義為

<img width="228" alt="1" src="https://github.com/user-attachments/assets/38d1eb29-dc73-4a43-92af-85066d244e2b" />

根據 Table 的估計值，得到的 t 值分別為：

<img width="231" alt="2" src="https://github.com/user-attachments/assets/458f6ad3-af7b-4147-9f75-7d1b1b8656b8" />


結果顯示，教師年資（TCHEXPER） 的係數在固定效果與隨機效果模型間存在顯著差異。

注意：此檢定可能出現問題，BOY 變數就是一例。若固定效果模型的標準誤小於隨機效果模型，則分母會變成負數的平方根，無法計算。這是因為標準誤本身為「估計變異數的平方根」，具有抽樣波動，有時大於真實變異數，有時小於。

Hausman 對比檢定（同時檢定六個係數）

檢定統計量：Hausman=22.62  , 自由度：6（六個係數） ,  臨界值：𝜒0.95,62=12.592

​由於 22.62 > 12.592 ，拒絕虛無假設：「未觀測的異質性與自變數無關」。因此，根據 Hausman 檢定，不建議使用隨機效果估計量；固定效果模型較為適當。


**(f)** Mundlak 模型與六個「校級均值」的聯合 F/χ² 檢定 (13.54)

Table  最後一欄列示 Mundlak 方程式的估計結果，其中 BOYM 的係數在 5% 顯著水準下顯著。對六個「校級平均變數」進行聯合顯著性檢定，得到的統計量為 13.54，略高於 𝜒(0.95,6)2=12.592。 因此，本題與 (e) 小題得到相同的結論──隨機效果估計量仍受到未觀測異質性與解釋變數相關的影響，不建議採用。







f. Create school-averages of the variables and carry out the Mundlak test for correlation between them and the unobserved heterogeneity.


#15.20 
![image](https://github.com/user-attachments/assets/af256bd5-3699-48de-8b66-8c27f3850960)
