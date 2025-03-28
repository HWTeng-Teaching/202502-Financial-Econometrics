rm(list = ls())

# 🔗 下載並載入 cocaine 資料集
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/cocaine.rdata",
              destfile = temp_file, mode = "wb")
load(temp_file)
head(cocaine)

#A#####################
# b1 截距應 > 0，b1 > 0
# b2 在正常經假假設下，價量應是負相關，因此 b2 < 0
# b3 可能表示越純或是品質越好的價格越高，b3 > 0
# b4 時間序列的因素應該趨近於0，推測在這段期間價格持平，b4 = 0
#B#####################
model <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model)

# b1 當quan t =0、qual = 0、trend = 0 時，預測的 price 約為 90.85 美元／克。
#在真實情境中 quant=0 沒有意義（不會賣 0 克），但可視為模型的基準值。

# b2 其他條件不變時，每多賣 1 克，價格 (每克) 平均下降 0.06 美元。
#p-value 非常小 (2.85e-07) ⇒ 顯著
#解釋：存在折扣現象，量越大單價越便宜。

#b3在 quant、trend 固定時，純度每增加 1%，價格平均上升 0.116 美元／克。
#但 p-value=0.57，表示「無法拒絕純度對價格無影響的假設」，統計上不顯著。

# b4 與預期不符 報表顯示係數為負，表示這段期間內古柯鹼有下跌的趨勢

#C######################
#用 R squared 來判讀，50,97% 的變異可以被模型給解釋
#D#######################
# 1. 整份係數報表
coef_table <- summary(model)$coefficients
coef_table
# 2. 對應 "quant" 這個係數的行
beta_hat <- coef_table["quant", "Estimate"]     # 估計量
se_hat   <- coef_table["quant", "Std. Error"]   # 標準誤
t_val    <- coef_table["quant", "t value"]      # t 統計量
df       <- summary(model)$df[2]                # 殘差自由度

coef_table

# 2. 單尾 p-value (左尾)
p_value_left <- pt(t_val, df = df)
p_value_left
# 3. 輸出結果
cat("beta_hat =", beta_hat, "\n")
cat("t_val =", t_val, "\n")
cat("df =", df, "\n")
cat("One-sided p-value (H1: beta < 0) =", p_value_left, "\n")
cat("p-value < 0.05 b2顯著 < 0")
#E############################
# df = 52 t0 = 0.572
p_right <- 1 - pt(0.572, df = 52)
p_right
#0 p value = 0.285 > 0.05 reject H0 ，品質的高低對於價格並沒有顯著的正向影響
#F#############################
#在此期間價格呈下降趨勢，可能有以下原因：
#供給增加：
#毒品市場若供給端增強（例如生產效率提升、走私通路更多），在需求大致穩定的情況下，會造成價格下跌。
#執法或風險溢價變化：
#或許執法打擊的方式改變、交易風險分散，導致「溢價」不如以往高，價格下降。
#市場競爭加劇：
#如果同地區出現更多經銷商，為了爭奪市場份額，會降低價格吸引買家。
#需求減少：
#若當地消費者對古柯鹼需求趨緩，或有其他替代品出現，也可能導致價格下降。
#在實際研究中，常需要結合歷史政策、執法力度、經濟條件等資訊，才能更具體說明「為何」價格會在 1984~1991 之間呈現下滑趨勢。
