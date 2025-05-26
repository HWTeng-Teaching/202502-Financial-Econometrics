if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
library(POE5Rdata)
library(ggplot2)
install.packages(c("tidyverse","AER","systemfit","broom"))
library(AER)         
library(systemfit)  
library(tidyverse)
library(broom)
library(dplyr)
install.packages("plm") 
library(plm)  
data('star')

# (a) 所有變數對成績的影響
model <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model)

# (b) 固定學校、學生後的結果
pdata <- pdata.frame(star, index = c("schid", "id"))

# 估計固定效果模型：加入學校固定效果
fe_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                data = pdata, model = "within", effect = "individual")

summary(fe_model)

# (c) 檢驗固定學校的效果
pooled_model <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                    data = pdata, model = "pooling")

pFtest(fe_model, pooled_model)
# (d) 檢驗固定效果是否顯著
re_model <- plm(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data  = pdata,
  model = "random"
)
summary(re_model)

plmtest(
  readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
  data   = pdata,
  type   = "bp",
  effect = "individual"
)

# (e) Hausmen Test
hausman_test <- phtest(fe_model, re_model)
hausman_test

# (f) Mundlak test and school-Average
library(plm)
library(lmtest)
library(sandwich)

data("star", package="plm")

star$mean_small        <- ave(star$small,        star$schid, FUN = mean, na.rm = TRUE)
star$mean_aide         <- ave(star$aide,         star$schid, FUN = mean, na.rm = TRUE)
star$mean_tchexper     <- ave(star$tchexper,     star$schid, FUN = mean, na.rm = TRUE)
star$mean_white_asian  <- ave(star$white_asian,  star$schid, FUN = mean, na.rm = TRUE)
star$mean_freelunch    <- ave(star$freelunch,    star$schid, FUN = mean, na.rm = TRUE)

library(plm)
pdata_mundlak <- pdata.frame(star, index = c("schid", "id"))

# 建立 Mundlak 模型：包含校內差異與學校平均
mundlak_model <- plm(mean_small + mean_aide + mean_tchexper + mean_white_asian + mean_freelunch,
  data = pdata_mundlak,
  model = "random"
)



