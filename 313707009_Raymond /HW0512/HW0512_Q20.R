if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata", force = TRUE)
install.packages("car")  
library(car)
install.packages("AER")  
library(AER)
install.packages("plm")  
library(plm)
library(POE5Rdata)
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

# 檢定學校固定效果是否顯著
pFtest(fe_model, pooled_model)


