#### functions.R ####

# This file includes functions used for the main R scripts

summarize2 <- function(x, na.rm = TRUE) {
  result <- c(
    Min = min(x, na.rm = na.rm),
    Q25 = quantile(x, probs = 0.25, na.rm = na.rm),
    Median = median(x, na.rm = na.rm),
    Q75 = quantile(x, probs = 0.75, na.rm = na.rm),
    Max = max(x, na.rm = na.rm),
    Mean = mean(x, na.rm = na.rm),
    SD = sd(x, na.rm = na.rm),
    No_of_unique_values = n_distinct(x),
    N = length(x) - sum(is.na(x)),
    No_NA = sum(is.na(x))
  )
}
sum_stats <- c("Min", "1st Quar", "Median", "3rd Quar", "Max", "Mean", "SD", "No of unique values", "n", "No of NA")