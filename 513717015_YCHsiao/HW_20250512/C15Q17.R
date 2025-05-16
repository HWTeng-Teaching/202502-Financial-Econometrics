# 安裝套件（如尚未安裝）
install.packages("remotes")
remotes::install_github("ccolonescu/POE5Rdata")

# 載入資料與套件
library(POE5Rdata)
library(plm)
library(lmtest)
library(sandwich)
library(dplyr)


# 查看套件中有哪些資料
data(package = "POE5Rdata")

data("liquor5", package = "POE5Rdata")
head(liquor5)
str(liquor5)


########a

liquor_fd <- liquor5 %>%
  arrange(hh, year) %>%  # 確保先按 household 排好
  group_by(hh) %>%
  mutate(
    LIQUORD = liquor - lag(liquor),
    INCOMED = income - lag(income)
  ) %>%
  filter(!is.na(LIQUORD))

fd_model <- lm(LIQUORD ~ INCOMED - 1, data = liquor_fd)
summary(fd_model)
confint(fd_model)

summary(liquor_fd$LIQUORD)
summary(liquor_fd$INCOMED)

# 再看看唯一值數量：
length(unique(liquor_fd$LIQUORD))
length(unique(liquor_fd$INCOMED))


################b.
library(plm)

# 建立 panel data 格式
pdata <- pdata.frame(liquor5, index = c("hh", "year"))

# 隨機效果模型
re_model <- plm(liquor ~ income, data = pdata, model = "random")
summary(re_model)

# 加上 robust 標準誤
library(lmtest)
library(sandwich)

coeftest(re_model, vcov = vcovHC(re_model, type = "HC1"))




################c.

plmtest(re_model, type = "bp")



################d.
# 建立每個 household 的平均 income
liquor_mundlak <- liquor5 %>%
  group_by(hh) %>%
  mutate(INCOMEM = mean(income)) %>%
  ungroup()

pdata_mundlak <- pdata.frame(liquor_mundlak, index = c("hh", "year"))

# RE 模型中加入 INCOMEM：若顯著 → 違反 RE 假設 → 應選 FE
mundlak_model <- plm(liquor ~ income + INCOMEM, data = pdata_mundlak, model = "random")
summary(mundlak_model)

