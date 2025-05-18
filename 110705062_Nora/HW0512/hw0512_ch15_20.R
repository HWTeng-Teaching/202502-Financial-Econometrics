url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/star.rdata"
file_path <- "star.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)  # 載入資料框 star
library(dplyr)
library(plm)

##15.20.1
model_a <- lm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch, data = star)
summary(model_a)

##15.20.2
pdata <- pdata.frame(star, index = c("schid", "id"))
model_b <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
               data = pdata, model = "within", effect = "individual")  # school FE
summary(model_b)

##15.20.3
model_pooled <- plm(readscore ~ small + aide + tchexper + boy + white_asian + freelunch,
                    data = pdata, model = "pooling")
pFtest(model_b, model_pooled)
