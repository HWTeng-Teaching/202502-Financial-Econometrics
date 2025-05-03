rm(list = ls()) 
temp_file <- tempfile(fileext = ".rdata")
download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/mroz.rdata", 
              destfile = temp_file, 
              mode = "wb")
load(temp_file)
mroz

# a

mroz$mothercoll <- ifelse(mroz$mothereduc > 12, 1, 0)
mroz$fathercoll <- ifelse(mroz$fathereduc>12, 1, 0)
anyparentcoll <- ifelse(mroz$mothercoll == 1 | mroz$fathercoll == 1, 1, 0)
percent_college_parents <- mean(anyparentcoll, na.rm = TRUE) * 100
round(percent_college_parents, 4) # 16.3347%

# b

cor(mroz[, c("educ", "mothercoll", "fathercoll")], use = "complete.obs")
--由相關係數可知當太太父母的教育程度對太太的教育程度有正向影響。
--使用虛擬變數當作工具變數比用老婆父母的教育年數還好是因為可以滿足限制條件，
--因為比較不能細緻地捕捉到父母教育程度的影響，像是文化程度等等所以比較符合工具變數的定義。

# c

mroz_clean <- subset(mroz, !is.na(wage) & wage > 0 & !is.na(exper) & !is.na(educ) & !is.na(mothercoll))
iv_model <- ivreg(log(wage) ~ exper + I(exper^2) + educ | exper + I(exper^2) + mothercoll, data = mroz_clean)
summary(iv_model, diagnostics = TRUE)
coef_educ <- coef(iv_model)["educ"]
se_educ <- sqrt(vcov(iv_model)["educ", "educ"])
lowerci <- round(coef_educ - 1.96 * se_educ, 4)
upperci <- round(coef_educ + 1.96 * se_educ, 4)
cat("95% CI = [", lowerci,upperci, "]") 
--95% CI = [ -0.0012 0.1533 ]

# d

stage1_model <- lm(educ ~ exper + I(exper^2) + mothercoll, data = mroz_clean)
summary(stage1_model)
--因爲係數跟Ｆ檢定的 p-value 小於 0.05，所以 mothercoll 是強工具變數

# H0: mothercoll has no effect on educ
anova(lm(educ ~ exper + I(exper^2), data = mroz_clean),
      lm(educ ~ exper + I(exper^2) + mothercoll, data = mroz_clean))
--因爲 p-value<0.05, 所以增加 mothercoll 可以增加模型解釋力。

# e

iv_model_2iv <- ivreg(log(wage) ~ exper + I(exper^2) + educ |
                        exper + I(exper^2) + mothercoll + fathercoll, data = mroz_clean)
summary(iv_model_2iv, diagnostics = TRUE)
coef_educ <- coef(iv_model_2iv)["educ"]
se_educ   <- sqrt(vcov(iv_model_2iv)["educ", "educ"])
lower_95  <- round(coef_educ - 1.96 * se_educ, 4)
upper_95  <- round(coef_educ + 1.96 * se_educ, 4)
cat("95% CI = [", lower_95,upper_95, "]") # 95% CI = [ 0.0275 0.1482 ]
--upperci-lowerci = 0.1545
--upper_95-lower_95 = 0.1207
-> 用兩個工具變數 mothercoll 跟 fathercoll 的區間估計區間比較小

# f

stage1_model_2iv <- lm(educ ~ exper + I(exper^2) + mothercoll + fathercoll, data = mroz_clean)
summary(stage1_model_2iv)
model_reduced <- lm(educ ~ exper + I(exper^2), data = mroz_clean)
anova(model_reduced, stage1_model_2iv)
--由以上檢定的 p-value 可知模型會增加解釋力

# g

summary(iv_model_2iv, diagnostics = TRUE)
--Sargan 檢定，其卡方統計量為 0.238，p 值為 0.626，遠大於 0.05。
--這表示我們無法拒絕虛無假設，也就是「多出來的工具變數（FATHERCOLL）是有效的、與誤差項無關」。
--結論： FATHERCOLL 這個工具變數是有效的，模型滿足過度識別檢定，整體工具變數設計是合理的。






