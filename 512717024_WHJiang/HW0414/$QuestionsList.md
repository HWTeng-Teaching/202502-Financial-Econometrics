<img width="577" alt="C08Q06" src="https://github.com/user-attachments/assets/781cb6e3-d84f-45d8-bcda-aec4bf015424" />


請考慮以下薪資方程式：

WAGEᵢ = β₁ + β₂EDUCᵢ + β₃EXPERᵢ + β₄METROᵢ + eᵢ

其中 WAGE 是每小時薪資（美元），EDUC 和 EXPER 為年數，METRO = 1 代表該人居住在都會區。資料來自 2013 年，共有 N = 1000 筆觀測值。

a. 我們想知道在控制教育、經驗與居住地變數（METRO）後，男性與女性薪資的隨機變異量是否相同。假設 var(eᵢ|xᵢ, FEMALE = 0) = σ²ₘ，var(eᵢ|xᵢ, FEMALE = 1) = σ²𝒻，欲檢定虛無假設 σ²ₘ = σ²𝒻，對立假設為 σ²ₘ ≠ σ²𝒻。根據 577 筆男性資料，得 SSEₘ = 97161.9174；女性資料推估 σ̂𝒻² = 12.024。請在 5% 顯著水準下進行檢定，列出檢定統計量與拒絕區域，並說明結論。

b. 假設已婚者因伴侶支持能嘗試更多就業選項，因此薪資變異可能較大。假設 var(eᵢ|xᵢ, MARRIED = 0) = σ²SINGLE，var(eᵢ|xᵢ, MARRIED = 1) = σ²MARRIED，欲檢定 σ²SINGLE = σ²MARRIED 對 σ²MARRIED > σ²SINGLE。我們將 FEMALE 納入迴歸式：WAGEᵢ = β₁ + β₂EDUCᵢ + β₃EXPERᵢ + β₄METROᵢ + β₅FEMALEᵢ + eᵢ。單身樣本 N = 400，SSE = 56231.0382；已婚樣本 N = 600，SSE = 100703.0471。請在 5% 顯著水準下檢定並說明結論。

c. 根據 (b) 的回歸，進行 NR² 檢定，檢定異質變異數。統計量為 59.03，請在 5% 顯著水準下做結論。這是否支持 (b) 中對誤差變異差異的討論？請說明。

d. 根據 (b) 的回歸，執行 White 檢定。統計量為 78.82，請問其自由度與 5% 臨界值是多少？並做結論。

e. (b) 的迴歸模型其 OLS 與 robust 標準誤如下：WAGE = –17.77 + 2.50EDUC + 0.23EXPER + 3.23METRO – 4.20FEMALE (se)     (2.36)   (0.14)     (0.031)   (1.05)     (0.81) (robse)  (2.50)   (0.16)     (0.029)   (0.84)     (0.80) 哪些係數的區間估計變窄？哪些變寬？是否有不一致的情形？

f. 若在 (b) 模型中加入 MARRIED 變數，其 White 修正後 t 值約為 1.0。此結果是否與 (b) 中的異質變異數檢定一致？請說明。


---------


<img width="585" alt="C08Q16" src="https://github.com/user-attachments/assets/41b22ed3-b14a-4962-93d9-5c0e3f16b254" />


<img width="598" alt="C08Q18" src="https://github.com/user-attachments/assets/c3bad38a-2eb3-44ec-8ae0-3370bf6cc388" />
