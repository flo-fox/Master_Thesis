#### functions.R ####

# This file includes functions and options used for the main R scripts

#### Options ####
options(scipen = 20)
theme_set(theme_minimal())
p_value_cutoff <- 0.01

#### Functions ####

#### Customed summarize() function
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


#### LaTeX export text
tex_control_text <- "Controls, \\ac{FE} and \\ac{SE} as indicated in the respective column. Controls, if included and as long as not perfectly collinear: population, share of population over 65 years, unemployment rate, total area, share of working-age population, and dummies for ``district-free'' cities, cities, as well as year in the electoral cycle."
tex_signif_text <- "Significance codes: *** p < 0.001, ** p < 0.01, * p < 0.05, ' p < 0.10.}"

#### Function to generate significance stars out of p values ####
assign_stars <- function(p) {
  if (p < 0.001) {
    print("***")
  }
  else if (p < 0.01) {
    print("**")
  }
  else if (p < 0.05) {
    print("*")
  }
  else if (p < 0.1) {
    print("'")
  }
}

#### Function to label p values ####
# tweaked from https://stackoverflow.com/questions/23018256/printing-p-values-with-0-001
p_value <- function(pvals, sig.limit = .001, digits = 3, cutoff_more_detailed = .1) {
  
  roundr <- function(x, digits = 1) {
    res <- sprintf(paste0('%.', digits, 'f'), x)
    zzz <- paste0('0.', paste(rep('0', digits), collapse = ''))
    res[res == paste0('-', zzz)] <- zzz
    res
  }
  
  sapply(pvals, function(x, sig.limit) {
    if (x < sig.limit)
      return(sprintf('p<%s', format(sig.limit)))
    if (x > cutoff_more_detailed)
      return(paste0("p=", roundr(x, digits = 2))) else
        return(paste0("p=", roundr(x, digits = digits)))
  }, sig.limit = sig.limit)
}

#### Rounding of decimals ####
round_decimals <- 4
