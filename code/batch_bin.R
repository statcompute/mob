batch_bin <- function(data, method) {
# INPUT
# data  : input dataframe with Y in the last column
# method: an integer representing the binning algorithm, currently:
#         1 for the iteration, 2 for the iteration for bads only
#         3 for the isotonic regression, 4 for the generalized boosting model

  if (sum(Reduce(c, Map(is.numeric, data))) < ncol(data)) {
    stop("All variables in the data frame need to be numeric !")
  }

  mlist <- c("qtl_bin", "bad_bin", "iso_bin", "gbm_bin")
  if (method %in% 1:length(mlist)) {
    source(paste(mlist[method], ".R", sep = ''))
    bin_fn <- get(mlist[method])
  } else {
    stop("The method is not supported !")
  }

  df1 <- subset(data, data[, ncol(data)] %in% c(0, 1))
  xnames <- colnames(df1)[1:(ncol(df1) - 1)]
  yname <- colnames(df1)[ncol(df1)]

  xlst <- parallel::mclapply(xnames, mc.cores = parallel::detectCores(), 
            function(xname) bin_fn(data.frame(y = df1[[yname]], x = df1[[xname]]), y, x))
  names(xlst) <- xnames

  xsum <- Reduce(rbind, parallel::mclapply(xnames, mc.cores = parallel::detectCores(),
            function(xname) data.frame(var    = xname,
                                       nbin   = nrow(xlst[[xname]]$df),
                                       unique = length(unique(df1[!is.na(df1[[xname]]), xname])),
                                       miss   = sum(xlst[[xname]]$df[["mv_cnt"]]),
                                       min    = min(df1[!is.na(df1[[xname]]), xname]),
                                       median = median(df1[!is.na(df1[[xname]]), xname]),
                                       max    = max(df1[!is.na(df1[[xname]]), xname]),
                                       ks     = max(xlst[[xname]]$df[["ks"]]),
                                       iv     = sum(xlst[[xname]]$df[["iv"]]))))

  BinOut <- list(BinSum = xsum, BinLst = xlst)
  class(BinOut) <- "BinSummary"
  return(BinOut)
}

print.BinSummary <- function(x) {
  print(knitr::kable(x$BinSum, padding = 2))
}
