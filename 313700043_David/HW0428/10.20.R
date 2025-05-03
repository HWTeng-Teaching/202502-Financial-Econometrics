rm(list = ls()) 
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
capm5

# a

capm5$msft_excess <- capm5$msft - capm5$riskfree
capm5$mkt_excess  <- capm5$mkt - capm5$riskfree
model_msft <- lm(msft_excess ~ mkt_excess, data = capm5)
summary(model_msft)
coef of alpha = 0.0033
coef of beta = 1.2018
因爲 beta>1, 所以 Microsoft 的股票是 risky 的

# b

capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm5$rank <- rank(capm5$mkt_excess, ties.method = "first")
stage1_rank <- lm(mkt_excess ~ rank, data = capm5)
summary(stage1_rank)
--rank 係數檢定的 p-value 小於0.05, 且 F 檢定值大於 10 , 加上 R square 有 91.21 %, 
--因此 rank 是一個強的工具變數

# c

stage1_rank <- lm(mkt_excess ~ rank, data = capm5)
capm5$resid_v <- residuals(stage1_rank)
hausman_model <- lm(msft_excess ~ mkt_excess + resid_v, data = capm5)
summary(hausman_model)
--因爲 resid_v 的 p-value 大於 0.01, 無法拒絕虛無假設，有證據支持誤差項具有外生性

# d

iv_model_rank <- ivreg(msft_excess ~ mkt_excess | rank, data = capm5)
summary(iv_model_rank)
--使用 IV 估計的 beta 值為1.2783，使用 OLS 的估計值為1.2018，
--這與我們的預期一致，因為若市場報酬具有內生性，OLS 估計將偏誤（可能向下偏）。
--IV 模型可修正這項偏誤，提供更一致的估計。

# e

capm5$mkt_excess <- capm5$mkt - capm5$riskfree
capm5$rank <- rank(capm5$mkt_excess, ties.method = "first")
capm5$pos <- ifelse(capm5$mkt_excess > 0, 1, 0)
stage1_model <- lm(mkt_excess ~ rank + pos, data = capm5)
summary(stage1_model)

--在第一階段回歸中，我們使用 rank 和 pos 作為工具變數來解釋市場超額報酬（mkt_excess），
--回歸結果顯示：rank 的係數非常顯著（t = 24.55, p < 2e-16）
--pos 也達到 5% 顯著水準（t = -2.20, p = 0.0291), F 檢定統計量為 951.3，p-value 小於 2.2e-16，
--表示這兩個工具變數在統計上「joint significant」
--第一階段回歸的 R^2 = 0.9149，代表這組 IV 對市場超額報酬具有非常強的解釋力
--綜合來看，rank 與 pos 可被視為強工具變數（strong IVs）。

# f

iv_model <- ivreg(msft_excess ~ mkt_excess | rank + pos, data = capm5)
summary(iv_model)
ols_model <- lm(msft_excess ~ mkt_excess, data = capm5)
summary(ols_model)
--使用 IV 估計的 beta 值為1.2831，使用 OLS 的估計值為1.2018，
--工具變數估計結果合理，且支持 CAPM 假說中市場報酬為主要解釋因子
--市場報酬變數（mkt_excess）可視為外生變數；
--RANK 與 POS 可作為有效且顯著的工具變數



















