url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata"
file_path <- "mroz.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
nrow(mroz)


mroz_lfp <- subset(mroz, lfp == 1)
nrow(mroz_lfp)
mroz_lfp$MOTHERCOLL <- as.numeric(mroz_lfp$mothereduc > 12)
mroz_lfp$FATHERCOLL <- as.numeric(mroz_lfp$fathereduc > 12)

# 計算比例
mean(mroz_lfp$MOTHERCOLL) * 100  # 母親有大學教育的比例 (%)
mean(mroz_lfp$FATHERCOLL) * 100  # 父親有大學教育的比例 (%)

cor(mroz_lfp[, c("educ", "MOTHERCOLL", "FATHERCOLL")], use = "complete.obs")

#install.packages("AER")
library(AER)

iv1 <- ivreg(
  log(wage) ~ educ + exper + I(exper^2) |    # 被解釋 + 內生 + 外生
    MOTHERCOLL + exper + I(exper^2),     # 工具 + 外生
  data = mroz_lfp
)
summary(iv1, diagnostics = TRUE)     # diagnostics 顯示弱工具檢定等


# 顯示 educ 的 95% 信賴區間
confint(iv1, level = 0.95)["educ", ]

#beta1  <- coef(iv1)["educ"]
#se1    <- sqrt(vcovHC(iv1, type = "HC1")["educ","educ"])
#ci95_1 <- beta1 + c(-1.96, 1.96)*se1
#ci95_1

first_stage_c <- lm(educ ~ MOTHERCOLL+ exper + I(exper^2), data = mroz_lfp)
summary(first_stage_c)  # 看 F 統計值與 p-value

linearHypothesis(first_stage_c, c("MOTHERCOLL = 0"))



iv_model_e <- ivreg(log(wage) ~ educ + exper + I(exper^2) | MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz_lfp)
summary(iv_model_e)
confint(iv_model_e, level = 0.95)["educ", ]

### f. First stage with both instruments
first_stage_f <- lm(educ ~ MOTHERCOLL + FATHERCOLL + exper + I(exper^2), data = mroz_lfp)
summary(first_stage_f)

# F-test for joint significance


# 使用 car 套件做 F 檢定

library(car)

# 檢定 MOTHERCOLL 和 FATHERCOLL 是否聯合為 0（joint significance test）
linearHypothesis(first_stage_f, c("MOTHERCOLL = 0", "FATHERCOLL = 0"))

summary(iv_model_e, diagnostics = TRUE)