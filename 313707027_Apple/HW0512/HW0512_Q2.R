# 15.17
library(POE5Rdata)
data(liquor5)

# Sort the data by household(hh) and year to ensure correct differencing
liquor5 <- liquor5[order(liquor5$hh, liquor5$year), ]

# Create first-differenced variables for LIQUOR and INCOME within each household
liquor5$LIQUORD <- ave(liquor5$liquor, liquor5$hh, FUN = function(x) c(NA, diff(x)))
liquor5$INCOMED <- ave(liquor5$income, liquor5$hh, FUN = function(x) c(NA, diff(x)))

# Remove rows with NA values
diff_data <- na.omit(liquor5[, c("LIQUORD", "INCOMED")])

model <- lm(LIQUORD ~ INCOMED - 1, data = diff_data)
summary(model)
confint(model, level = 0.95)
