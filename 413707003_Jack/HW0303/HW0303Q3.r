# a

url <- "https://www.principlesofeconometrics.com/poe5/data/rdata/cex5_small.rdata"
load(url(url))

ggplot(cex5_small, aes(x = foodaway)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of foodaway",
       x = "Food Away from Home Expenditure (per person, per month)",
       y = "Frequency") +
  theme_minimal()


summary_stats <- summary(cex5_small$foodaway)
summary_stats

# Mean 49.27
# Median 32.55
# 25th percentiles 12.04
# 75th percentiles 67.50


# b

cex5_small$education_group <- ifelse(cex5_small$advanced == 1, "Advanced Degree",
                                     ifelse(cex5_small$college == 1, "College Degree", "No College Degree"))

education_stats <- aggregate(foodaway ~ education_group, data = cex5_small, 
                             FUN = function(x) c(mean = mean(x), median = median(x)))

education_stats <- do.call(data.frame, education_stats)
colnames(education_stats) <- c("Education Group", "Mean Foodaway", "Median Foodaway")


print(education_stats)

#     Education Group Mean Foodaway Median Foodaway
# 1   Advanced Degree      73.15494           48.15
# 2    College Degree      48.59718           36.11
# 3 No College Degree      39.01017           26.02



