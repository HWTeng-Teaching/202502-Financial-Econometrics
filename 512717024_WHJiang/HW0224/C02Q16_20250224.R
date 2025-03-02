# C02Q16

rm(list=ls()) # Caution: this clears the Environment

library(devtools)
library(PoEdata)
library(bookdown)
library(knitr)
library(xtable)
library(printr)
library(stargazer)
library(rmarkdown)

# 載入 Rdata 資料檔案
temp_file <- tempfile(fileext = ".rdata")


download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata", 
              destfile = temp_file, 
              mode = "wb")  
load(temp_file)

# 檢查載入 Rdata 資料檔案
data("capm5")  # This is the way that we download the data
?capm5  # provides info for the dataset

capm5

# let's look around the data
# Check possible relationships between variables
# Think about possible models to use.

head(capm5)
tail(capm5)
nrow(capm5)

##########################################
# (b) 小題：建立各股票與市場的超額報酬變數
##########################################

# 1. 設定工作目錄 (依照你的檔案路徑修改)
# setwd("你的工作目錄路徑")

# 2. 讀取資料
# 假設 capm5 資料是 CSV 格式，且含有欄位：
# Date, GE, IBM, Ford, Microsoft, Disney, MKT, RISKFREE
# capm5 <- read.csv("capm5.csv", header = TRUE)

# 如果有日期欄位，可將其轉成日期格式 (假設欄位名稱為 "Date")
# capm5$Date <- as.Date(capm5$Date, format = "%Y-%m-%d")

# 3. 檢查讀取資料的結構
str(capm5)       # 查看資料框的結構
head(capm5)      # 查看前幾筆資料
summary(capm5)   # 簡易統計摘要

# 4. 建立各股票的超額報酬 r_{jm} = r_j - r_f

capm5$ge_excess        <- capm5$ge         - capm5$riskfree
capm5$ibm_excess       <- capm5$ibm        - capm5$riskfree
capm5$ford_excess      <- capm5$ford       - capm5$riskfree
capm5$msft_excess      <- capm5$msft       - capm5$riskfree
capm5$dis_excess       <- capm5$dis        - capm5$riskfree
capm5$xom_excess       <- capm5$xom        - capm5$riskfree

# 5. 建立市場的超額報酬 (r_m - r_f)
capm5$mkt_excess <- capm5$mkt - capm5$riskfree

# 6. 確認新變數是否正確生成
head(capm5[, c("date", "ge_excess", "ibm_excess", "ford_excess",  
               "msft_excess", "dis_excess", "xom_excess", "mkt_excess")])

# 7. 其他可選擇性檢查或統計描述
# summary(capm4$ge_excess)
# summary(capm4$mkt_excess)

# 到此為止，已經完成(b)小題所需的資料前處理：
# - 將六檔(capm5)股票的報酬率轉換為超額報酬 (r_{jm})
# - 將市場報酬率轉換為超額報酬 (r_m - r_f)
# 這些變數後續可用於後續(c)~(f)小題的迴歸分析。

##############################################
# 開始解(b)小題：對各股票做 CAPM 迴歸並輸出表格
##############################################



# Q16(b)-01. 逐一對六檔股票做 CAPM 迴歸: r_{j} - r_f = α_j + β_j (r_m - r_f) + e_j
lm_ge     <- lm(ge_excess   ~ mkt_excess, data=capm5)
lm_ibm    <- lm(ibm_excess  ~ mkt_excess, data=capm5)
lm_ford   <- lm(ford_excess ~ mkt_excess, data=capm5)
lm_msft   <- lm(msft_excess ~ mkt_excess, data=capm5)
lm_dis    <- lm(dis_excess  ~ mkt_excess, data=capm5)
lm_xom    <- lm(xom_excess  ~ mkt_excess, data=capm5)

# Q16(b)-2. 收集迴歸結果的估計值(係數)與標準誤，整理成與圖片相同格式的表格
#   alpha_hat (估計值), alpha_se (標準誤), beta_hat, beta_se, 以及樣本數 N

  res_table <- data.frame(
    row.names = c("alpha_hat", "(alpha_se)", "beta_hat", "(beta_se)", "N"),
    
    GE   = c(
      sprintf("%.6f", coef(lm_ge)[1]),
      sprintf("(%.4f)", coef(summary(lm_ge))[1,2]),
      sprintf("%.6f", coef(lm_ge)[2]),
      sprintf("(%.4f)", coef(summary(lm_ge))[2,2]),
      length(lm_ge$residuals)
    ),
    IBM  = c(
      sprintf("%.6f", coef(lm_ibm)[1]),
      sprintf("(%.4f)", coef(summary(lm_ibm))[1,2]),
      sprintf("%.6f", coef(lm_ibm)[2]),
      sprintf("(%.4f)", coef(summary(lm_ibm))[2,2]),
      length(lm_ibm$residuals)
    ),
    FORD = c(
      sprintf("%.6f", coef(lm_ford)[1]),
      sprintf("(%.4f)", coef(summary(lm_ford))[1,2]),
      sprintf("%.6f", coef(lm_ford)[2]),
      sprintf("(%.4f)", coef(summary(lm_ford))[2,2]),
      length(lm_ford$residuals)
    ),
    MSFT = c(
      sprintf("%.6f", coef(lm_msft)[1]),
      sprintf("(%.4f)", coef(summary(lm_msft))[1,2]),
      sprintf("%.6f", coef(lm_msft)[2]),
      sprintf("(%.4f)", coef(summary(lm_msft))[2,2]),
      length(lm_msft$residuals)
    ),
    DIS  = c(
      sprintf("%.6f", coef(lm_dis)[1]),
      sprintf("(%.4f)", coef(summary(lm_dis))[1,2]),
      sprintf("%.6f", coef(lm_dis)[2]),
      sprintf("(%.4f)", coef(summary(lm_dis))[2,2]),
      length(lm_dis$residuals)
    ),
    XOM  = c(
      sprintf("%.6f", coef(lm_xom)[1]),
      sprintf("(%.4f)", coef(summary(lm_xom))[1,2]),
      sprintf("%.6f", coef(lm_xom)[2]),
      sprintf("(%.4f)", coef(summary(lm_xom))[2,2]),
      length(lm_xom$residuals)
    )
  )
  
# Q16(b)-3. 查看表格
res_table


############
# 文字結論
############

cat("
結論：
從上表可以看出，各檔股票的 beta 值如下：
- GE    的 beta ~", round(coef(lm_ge)[2], 3), "
- IBM   的 beta ~", round(coef(lm_ibm)[2], 3), "
- FORD  的 beta ~", round(coef(lm_ford)[2], 3), "
- MSFT  的 beta ~", round(coef(lm_msft)[2], 3), "
- DIS   的 beta ~", round(coef(lm_dis)[2], 3), "
- XOM   的 beta ~", round(coef(lm_xom)[2], 3), "

若 beta > 1，代表對市場波動較敏感，屬於較『積極型』的股票；若 beta < 1，則較『防禦型』。
可以發現：
- FORD, GE, MSFT 的 beta > 1，顯示較積極，其中 FORD 的 beta ~ 1.662，最為積極。
- IBM, DIS, XOM 的 beta < 1，屬較防禦型，其中 XOM 的 beta ~ 0.457，最為防禦。
")

###################################################################
# 開始解(c) 小題：針對 Microsoft 股票檢視截距是否接近零，
#並繪製散佈圖與擬合迴歸線

#此段程式碼先利用 summary(lm_msft) 來檢查 Microsoft 模型中的截距
#估計值及其統計顯著性。接著利用 plot() 繪製市場超額報酬與 Microsoft 
#超額報酬的散點圖，並以 abline() 疊加出由 lm_msft 模型擬合出的迴歸線，
#方便視覺化檢查模型的適配情形。
###################################################################

# 檢視 Microsoft 迴歸結果摘要，確認 α (截距) 是否接近零
summary(lm_msft)

# 繪製 Microsoft 的超額報酬 (msft_excess) 與市場超額報酬 (mkt_excess) 的散佈圖
plot(capm5$mkt_excess, capm5$msft_excess,
     main = "Microsoft: Excess Return vs. Market Excess Return",
     xlab = "Market Excess Return (MKT - RISKFREE)",
     ylab = "Microsoft Excess Return (msft - riskfree)",
     pch = 16, col = "blue")

# 疊加 Microsoft 的迴歸線（來自 lm_msft 模型）
abline(lm_msft, col = "red", lwd = 2)

###################################################################
# 開始解(d) 小題：針對每檔股票估計不含截距 (α = 0) 的模型
# (強制截距為 0 估計模型，並比較 β 值變化)
###################################################################

# 以無截距模型重新估計每檔股票的 CAPM 模型
lm_ge_noint   <- lm(ge_excess   ~ 0 + mkt_excess, data = capm5)
lm_ibm_noint  <- lm(ibm_excess  ~ 0 + mkt_excess, data = capm5)
lm_ford_noint <- lm(ford_excess ~ 0 + mkt_excess, data = capm5)
lm_msft_noint <- lm(msft_excess ~ 0 + mkt_excess, data = capm5)
lm_dis_noint  <- lm(dis_excess  ~ 0 + mkt_excess, data = capm5)
lm_xom_noint  <- lm(xom_excess  ~ 0 + mkt_excess, data = capm5)

# 顯示 Microsoft 無截距模型結果，方便檢查 β 值
summary(lm_msft_noint)

# 將有截距與無截距模型的 β 值整理比較
beta_original <- c(
  GE   = coef(lm_ge)[2],
  IBM  = coef(lm_ibm)[2],
  FORD = coef(lm_ford)[2],
  MSFT = coef(lm_msft)[2],
  DIS  = coef(lm_dis)[2],
  XOM  = coef(lm_xom)[2]
)

beta_noint <- c(
  GE   = coef(lm_ge_noint)[1],
  IBM  = coef(lm_ibm_noint)[1],
  FORD = coef(lm_ford_noint)[1],
  MSFT = coef(lm_msft_noint)[1],
  DIS  = coef(lm_dis_noint)[1],
  XOM  = coef(lm_xom_noint)[1]
)

# 將 β 值整理成資料框進行比較
beta_comparison <- data.frame(beta_original, beta_noint)
print(beta_comparison)
