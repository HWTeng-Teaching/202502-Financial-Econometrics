url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cocaine.rdata"
file_path <- "cocaine.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(cocaine)

model <- lm(price ~ quant + qual + trend, data = cocaine)
summary(model)
