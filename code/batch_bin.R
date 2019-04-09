batch_bin <- function(data, method = "mono_bin") {
# INPUT
# data  : input dataframe with Y in the last column
# method: a character string indicating the method used for monotonic binning
#         one of "mono_bin", "iso_bin", or "gbm_bin"
# OUTPUT
# batch_bin(df, "mono_bin")
#          var nbin miss      ks     iv
#    tot_derog    3  213 18.9469 0.1975
#   ...SKIPPED...
#   tot_income    6    5 10.2171 0.0677

  ### GET THINGS READY ###
  df1 <- subset(data, data[, ncol(data)] %in% c(0, 1))
  xnames <- colnames(df1)[1:(ncol(df1) - 1)]
  yname <- colnames(df1)[ncol(df1)]
  bin_fn <- get(method)

  xlst <- parallel::mclapply(xnames, mc.cores = parallel::detectCores(), 
            function(xname) bin_fn(data.frame(y = df1[[yname]], x = df1[[xname]]), y, x))
  names(xlst) <- xnames

  xsum <- Reduce(rbind, parallel::mclapply(xnames, mc.cores = parallel::detectCores(),
            function(xname) data.frame(var  = xname,
                                       nbin = nrow(xlst[[xname]]$df),
                                       miss = sum(xlst[[xname]]$df[["mv_cnt"]]),
                                       ks   = max(xlst[[xname]]$df[["ks"]]),
                                       iv   = sum(xlst[[xname]]$df[["iv"]]))))

  BinOut <- list(BinSum = xsum, BinLst = xlst)
  class(BinOut) <- "BinSummary"
  return(BinOut)
}

print.BinSummary <- function(x) {
  print(x$BinSum)
}
