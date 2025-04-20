url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/commute5.rdata"
file_path <- "commute5.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(commute5)

model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)
confint(model, level = 0.95)
