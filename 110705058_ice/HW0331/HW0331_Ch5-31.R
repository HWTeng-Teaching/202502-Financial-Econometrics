url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/commute5.rdata"
file_path <- "commute5.rdata"
download.file(url, file_path, mode = "wb")
load(file_path)
ls()
head(commute5)

model <- lm(time ~ depart + reds + trains, data = commute5)
summary(model)
confint(model, level = 0.95)

qt(0.9,245)
qt(0.95,245)
qt(0.975,245)

vcov_matrix <- vcov(model)
vcov_matrix
c_vec <- c(0, 0, -3, 1)
sef <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec)
sef


c_vec <- c(1, 30, 6, 1)
sef <- sqrt(t(c_vec) %*% vcov_matrix %*% c_vec)
sef