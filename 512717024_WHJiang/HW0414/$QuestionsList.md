Regression Analysis Question 8.6
English Version:
Consider the wage equation:
WAGEᵢ = β₁ + β₂EDUCᵢ + β₃EXPERᵢ + β₄METROᵢ + eᵢ
where wage is measured in dollars per hour, education and experience are in years, and METRO = 1 if the person lives in a metropolitan area. We have N = 1000 observations from 2013.
a. We are curious whether holding education, experience, and METRO constant, there is the same amount of random variation in wages for males and females. Suppose var(eᵢ|xᵢ, FEMALE = 0) = σ²ₘ and var(eᵢ|xᵢ, FEMALE = 1) = σ²𝒻. We specifically wish to test the null hypothesis σ²ₘ = σ²𝒻 against σ²ₘ ≠ σ²𝒻. Using 577 observations on males, we obtain the sum of squared OLS residuals, SSEₘ = 97161.9174. The regression using data on females yields σ̂𝒻² = 12.024. Test the null hypothesis at the 5% level of significance. Clearly state the value of the test statistic and the rejection region, along with your conclusion.
b. We hypothesize that married individuals, relying on spousal support, can seek wider employment types and hence holding all else equal should have more variable wages. Suppose var(eᵢ|xᵢ, MARRIED = 0) = σ²SINGLE and var(eᵢ|xᵢ, MARRIED = 1) = σ²MARRIED. Specify the null hypothesis σ²SINGLE = σ²MARRIED versus the alternative hypothesis σ²MARRIED > σ²SINGLE. We add FEMALE to the wage equation as an explanatory variable, so that: WAGEᵢ = β₁ + β₂EDUCᵢ + β₃EXPERᵢ + β₄METROᵢ + β₅FEMALEᵢ + eᵢ. Using N = 400 observations on single individuals, OLS estimation yields a sum of squared residuals = 56231.0382. For the 600 married individuals, the sum of squared errors is 100,703.0471. Test the null hypothesis at the 5% level of significance. Clearly state the value of the test statistic and the rejection region, along with your conclusion.
c. Following the regression in part (b), we carry out the NR² test using the right-hand-side variables in (XR8.6b) as candidates related to the heteroskedasticity. The value of this statistic is 59.03. What do we conclude about heteroskedasticity, at the 5% level? Does this provide evidence about the issue discussed in part (b), whether the error variation is different for married and unmarried individuals? Explain.
d. Following the regression in part (b), we carry out the White test for heteroskedasticity. The value of the test statistic is 78.82. What are the degrees of freedom of the test statistic? What is the 5% critical value for the test? What do you conclude?
e. The OLS fitted model from part (b), with usual and robust standard errors, is: WAGE = –17.77 + 2.50EDUC + 0.23EXPER + 3.23METRO – 4.20FEMALE (se)     (2.36)   (0.14)     (0.031)   (1.05)     (0.81) (robse)  (2.50)   (0.16)     (0.029)   (0.84)     (0.80) For which coefficients have interval estimates gotten narrower? For which have interval estimates gotten wider? Is there an inconsistency in the results?
f. If we add MARRIED to the model in part (b), we find that its t-value using a White heteroskedasticity robust estimator is about 1.0. Does this conflict with, or is it compatible with, the result in (b) concerning heteroskedasticity? Explain.
繁體中文版本:
請考慮以下薪資方程式：
WAGEᵢ = β₁ + β₂EDUCᵢ + β₃EXPERᵢ + β₄METROᵢ + eᵢ
其中 WAGE 是每小時薪資（美元），EDUC 和 EXPER 為年數，METRO = 1 代表該人居住在都會區。資料來自 2013 年，共有 N = 1000 筆觀測值。
a. 我們想知道在控制教育、經驗與居住地變數（METRO）後，男性與女性薪資的隨機變異量是否相同。假設 var(eᵢ|xᵢ, FEMALE = 0) = σ²ₘ，var(eᵢ|xᵢ, FEMALE = 1) = σ²𝒻，欲檢定虛無假設 σ²ₘ = σ²𝒻，對立假設為 σ²ₘ ≠ σ²𝒻。根據 577 筆男性資料，得 SSEₘ = 97161.9174；女性資料推估 σ̂𝒻² = 12.024。請在 5% 顯著水準下進行檢定，列出檢定統計量與拒絕區域，並說明結論。
b. 假設已婚者因伴侶支持能嘗試更多就業選項，因此薪資變異可能較大。假設 var(eᵢ|xᵢ, MARRIED = 0) = σ²SINGLE，var(eᵢ|xᵢ, MARRIED = 1) = σ²MARRIED，欲檢定 σ²SINGLE = σ²MARRIED 對 σ²MARRIED > σ²SINGLE。我們將 FEMALE 納入迴歸式：WAGEᵢ = β₁ + β₂EDUCᵢ + β₃EXPERᵢ + β₄METROᵢ + β₅FEMALEᵢ + eᵢ。單身樣本 N = 400，SSE = 56231.0382；已婚樣本 N = 600，SSE = 100703.0471。請在 5% 顯著水準下檢定並說明結論。
c. 根據 (b) 的回歸，進行 NR² 檢定，檢定異質變異數。統計量為 59.03，請在 5% 顯著水準下做結論。這是否支持 (b) 中對誤差變異差異的討論？請說明。
d. 根據 (b) 的回歸，執行 White 檢定。統計量為 78.82，請問其自由度與 5% 臨界值是多少？並做結論。
e. (b) 的迴歸模型其 OLS 與 robust 標準誤如下：WAGE = –17.77 + 2.50EDUC + 0.23EXPER + 3.23METRO – 4.20FEMALE (se)     (2.36)   (0.14)     (0.031)   (1.05)     (0.81) (robse)  (2.50)   (0.16)     (0.029)   (0.84)     (0.80) 哪些係數的區間估計變窄？哪些變寬？是否有不一致的情形？
f. 若在 (b) 模型中加入 MARRIED 變數，其 White 修正後 t 值約為 1.0。此結果是否與 (b) 中的異質變異數檢定一致？請說明。

