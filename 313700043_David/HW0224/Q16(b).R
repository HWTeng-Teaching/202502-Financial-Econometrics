temp_file <- tempfile(fileext = ".rdata")

download.file(url = "https://www.principlesofeconometrics.com/poe5/data/rdata/capm5.rdata", 
              destfile = temp_file, 
              mode = "wb")

load(temp_file)
capm5

firms <- c("ge", "ibm", "ford", "msft", "dis", "xom")
market_col <- "mkt"  
riskfree_col <- "riskfree"

capm5$MKT_excess <- capm5[[market_col]] - capm5[[riskfree_col]]

results <- data.frame(Firm = firms, Beta = numeric(length(firms)))

for(i in 1:length(firms)) {
  firm_excess <- capm5[[firms[i]]] - capm5[[riskfree_col]]
  model <- lm(firm_excess ~ capm5$MKT_excess)
  results$Alpha[i] <- coef(model)[1]
  results$Beta[i] <- coef(model)[2]
}

print(results)

defensive_firm <- results$Firm[which.min(results$Beta)]
defensive_beta <- min(results$Beta)

aggressive_firm <- results$Firm[which.max(results$Beta)]
aggressive_beta <- max(results$Beta)

cat(defensive_firm, "Beta值 =", round(defensive_beta, 4), "\n")
cat(aggressive_firm, "Beta值 =", round(aggressive_beta, 4), "\n")





