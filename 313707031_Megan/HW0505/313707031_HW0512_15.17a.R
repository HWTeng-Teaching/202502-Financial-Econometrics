library(POE5Rdata)
data(liquor5)

liquor5 <- liquor5[order(liquor5$hh, liquor5$year), ]

liquor5$liquord <- with(liquor5, ave(liquor, hh, FUN = function(x) c(NA, diff(x))))
liquor5$incomed <- with(liquor5, ave(income, hh, FUN = function(x) c(NA, diff(x))))

df <- na.omit(liquor5[, c("liquord", "incomed")])

model <- lm(liquord ~ incomed - 1, data = df)

cat("\nOLS 回歸結果：\n")
print(summary(model))

conf_int <- confint(model, level = 0.95)
formatted_conf_int <- sprintf("%-10s %10.8f %10.8f", rownames(conf_int), conf_int[, 1], conf_int[, 2])

cat("\n95% 信賴區間：\n")
cat("          2.5 %        97.5 %\n")
cat(formatted_conf_int, sep = "\n")