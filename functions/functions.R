#### functions.R ####

# This file includes functions and options used for the main R scripts

#### Options ####
options(scipen = 20)
theme_set(theme_minimal())
p_value_cutoff <- 0.01
round_decimals <- 4 # Rounding of decimals

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
    return("***")
  }
  else if (p < 0.01) {
    return("**")
  }
  else if (p < 0.05) {
    return("*")
  }
  else if (p < 0.1) {
    return("'")
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



#### Modify rddensity::rdplotdensity function ####
rdplotdensity_with_theme <- function (rdd, X, plotRange = NULL, plotN = 10, plotGrid = c("es", 
                                                                                         "qs"), alpha = 0.05, type = NULL, lty = NULL, lwd = NULL, 
                                      lcol = NULL, pty = NULL, pwd = NULL, pcol = NULL, CItype = NULL, 
                                      CIuniform = FALSE, CIsimul = 2000, CIshade = NULL, CIcol = NULL, 
                                      bwselect = NULL, hist = TRUE, histBreaks = NULL, histFillCol = 3, 
                                      histFillShade = 0.2, histLineCol = "white", title = "", 
                                      xlabel = "", ylabel = "", legendTitle = NULL, legendGroups = NULL) 
{
  c <- rdd$opt$c
  p <- rdd$opt$p
  q <- rdd$opt$q
  hl <- rdd$h$left
  hr <- rdd$h$right
  kernel <- rdd$opt$kernel
  regularize <- rdd$opt$regularize
  nLocalMin <- rdd$opt$nLocalMin
  nUniqueMin <- rdd$opt$nUniqueMin
  massPoints <- rdd$opt$massPoints
  X <- as.vector(X)
  if (any(is.na(X))) {
    warning(paste(sum(is.na(X)), " missing ", switch((sum(is.na(X)) > 
                                                        1) + 1, "observation is", "observations are"), " ignored.\n", 
                  sep = ""))
    X <- X[!is.na(X)]
  }
  if (length(plotRange) == 0) {
    plotRange <- c(max(min(X), c - 3 * hl), min(max(X), 
                                                c + 3 * hr))
  }
  else if (length(plotRange) != 2) {
    stop("Plot range incorrectly specified.\n")
  }
  else if (plotRange[1] >= c | plotRange[2] <= c) {
    stop("Plot range incorrectly specified.\n")
  }
  if (length(plotN) == 0) {
    plotN <- c(10, 10)
  }
  else if (length(plotN) == 1) {
    plotN <- c(plotN, plotN)
  }
  else if (length(plotN) > 2) {
    stop("Number of grid points incorrectly specified.\n")
  }
  if (plotN[1] <= 1 | plotN[2] <= 1) {
    stop("Number of grid points incorrectly specified.\n")
  }
  if (length(plotGrid) == 0) {
    plotGrid <- "es"
  }
  else {
    plotGrid <- plotGrid[1]
  }
  if (!plotGrid %in% c("es", "qs")) {
    stop("Grid specification invalid.\n")
  }
  if (hist & is.null(histBreaks)) {
    temp_hist_n_l <- sum(X >= plotRange[1] & X < c)
    temp_hist_n_l <- ceiling(min(sqrt(temp_hist_n_l), 10 * 
                                   log(temp_hist_n_l)/log(10)))
    temp_hist_n_r <- sum(X <= plotRange[2] & X >= c)
    temp_hist_n_r <- ceiling(min(sqrt(temp_hist_n_r), 10 * 
                                   log(temp_hist_n_r)/log(10)))
    histBreaks <- c(seq(plotRange[1], c, length.out = temp_hist_n_l + 
                          1), seq(c, plotRange[2], length.out = temp_hist_n_r + 
                                    1)[2:(temp_hist_n_r + 1)])
  }
  scalel <- (sum(X <= c) - 1)/(length(X) - 1)
  scaler <- (sum(X >= c) - 1)/(length(X) - 1)
  if (plotGrid == "es") {
    gridl <- seq(plotRange[1], c, length.out = plotN[1])
    gridl[plotN[1]] <- c
    gridr <- seq(c, plotRange[2], length.out = plotN[2])
    gridr[1] <- c
  }
  else {
    gridl <- seq(mean(X <= plotRange[1]), mean(X <= c), 
                 length.out = plotN[1])
    gridl <- quantile(X, gridl)
    gridr <- seq(mean(X <= c), mean(X <= plotRange[2]), 
                 length.out = plotN[2])
    gridr <- quantile(X, gridr)
    gridl[plotN[1]] <- c
    gridr[1] <- c
  }
  if (!is.null(bwselect)) {
    if (bwselect %in% c("mse-dpi", "imse-dpi", "mse-rot", 
                        "imse-rot")) {
      Estl <- lpdensity::lpdensity(data = X[X <= c], grid = gridl, 
                                   bwselect = bwselect, p = p, q = q, v = 1, kernel = kernel, 
                                   scale = scalel, regularize = regularize, nLocalMin = nLocalMin, 
                                   nUniqueMin = nUniqueMin, massPoints = massPoints)
      Estr <- lpdensity::lpdensity(data = X[X >= c], grid = gridr, 
                                   bwselect = bwselect, p = p, q = q, v = 1, kernel = kernel, 
                                   scale = scaler, regularize = regularize, nLocalMin = nLocalMin, 
                                   nUniqueMin = nUniqueMin, massPoints = massPoints)
    }
    else {
      stop("Option bwselect incorrectly specified.\n")
    }
  }
  else {
    Estl <- lpdensity::lpdensity(data = X[X <= c], grid = gridl, bw = hl, 
                                 p = p, q = q, v = 1, kernel = kernel, scale = scalel, 
                                 regularize = regularize, nLocalMin = nLocalMin, 
                                 nUniqueMin = nUniqueMin, massPoints = massPoints)
    Estr <- lpdensity::lpdensity(data = X[X >= c], grid = gridr, bw = hr, 
                                 p = p, q = q, v = 1, kernel = kernel, scale = scaler, 
                                 regularize = regularize, nLocalMin = nLocalMin, 
                                 nUniqueMin = nUniqueMin, massPoints = massPoints)
  }
  Estplot <- lpdensity::lpdensity.plot(Estl, Estr, alpha = alpha, type = type, 
                                       lty = lty, lwd = lwd, lcol = lcol, pty = pty, pwd = pwd, 
                                       pcol = pcol, CItype = CItype, CIuniform = CIuniform, 
                                       CIsimul = CIsimul, CIshade = CIshade, CIcol = CIcol, 
                                       hist = hist, histData = X, histBreaks = histBreaks, 
                                       histFillCol = histFillCol, histFillShade = histFillShade, 
                                       histLineCol = histLineCol, title = title, xlabel = xlabel, 
                                       ylabel = ylabel, legendTitle = legendTitle, legendGroups = legendGroups) +
    theme_get() + # This line of code (as well as lpdensity:: package indicator above) added
    theme(legend.position = "none")
  print(Estplot)
  return(list(Estl = Estl, Estr = Estr, Estplot = Estplot))
}

