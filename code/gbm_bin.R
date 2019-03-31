gbm_bin <- function(data, y, x) {
# data: input dataframe
# y   : name of Y in the input dataframe
# x   : name of X in the input dataframe
#
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/manual_bin.R")
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/gbm_bin.R")
# gbm_bin(df, bad, ltv)
# $df
#    bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#     01                       $X <= 72  415 0.0711      0       33   0.0795 -1.0930 0.0598  5.4718
#     02             $X > 72 & $X <= 73   27 0.0046      0        3   0.1111 -0.7235 0.0019  5.7381
#     03             $X > 73 & $X <= 81  303 0.0519      0       35   0.1155 -0.6797 0.0194  8.5863
#     04             $X > 81 & $X <= 83  139 0.0238      0       17   0.1223 -0.6149 0.0074  9.7936
#     05             $X > 83 & $X <= 90  631 0.1081      0       85   0.1347 -0.5040 0.0235 14.4513
#     06             $X > 90 & $X <= 94  529 0.0906      0       89   0.1682 -0.2422 0.0049 16.4905
#     07             $X > 94 & $X <= 95  145 0.0248      0       26   0.1793 -0.1651 0.0006 16.8807
#     08            $X > 95 & $X <= 100  907 0.1554      0      198   0.2183  0.0804 0.0010 15.6024
#     09           $X > 100 & $X <= 101  195 0.0334      0       44   0.2256  0.1229 0.0005 15.1771
#     10           $X > 101 & $X <= 110 1217 0.2085      0      283   0.2325  0.1619 0.0057 11.6399
#     11           $X > 110 & $X <= 112  208 0.0356      0       50   0.2404  0.2054 0.0016 10.8637
#     12           $X > 112 & $X <= 115  253 0.0433      0       70   0.2767  0.3950 0.0075  8.9540
#     13           $X > 115 & $X <= 136  774 0.1326      0      226   0.2920  0.4702 0.0333  1.8655
#     14           $X > 136 | is.na($X)   94 0.0161      1       37   0.3936  0.9238 0.0172  0.0000
# $cuts
#  [1]  72  73  81  83  90  94  95 100 101 110 112 115 136

  ### GET THINGS READY ###
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- data[!is.na(data[, xname]), c(xname, yname)]
  df2 <- data.frame(y = df1[[yname]], x = df1[[xname]], x2 = df1[[xname]])

  ### DETECT THE CORRELATION DIRRECTION BETWEEN X AND Y ###
  cor <- cor(df2[, 2], df2[, 1], method = "spearman", use = "complete.obs")

  ### GET THE OUTPUT FROM A GENERALIZED BOOSTED REGRESSION MODEL ###
  mdl <- gbm::gbm(y ~ x + x2, distribution = "bernoulli", data = df2, var.monotone = c(cor / abs(cor), cor / abs(cor)), 
                  bag.fraction = 1, n.minobsinnode = round(nrow(df2) / 100), cv.folds = 10)
  best.n <- gbm::gbm.perf(mdl, method = "cv", plot.it = FALSE)
  df3 <- data.frame(y = df2$y, x = df2$x, yhat = gbm::predict.gbm(mdl, n.trees = best.n, type = "response"))
  
  ### AGGREGATE THE GBM OUTPUT ###
  df4 <- Reduce(rbind, 
           lapply(split(df3, df3$yhat), 
             function(x) data.frame(minx = min(x$x), 
                                    maxx = max(x$x),
                                    nobs = nrow(x),
                                    yavg = mean(x$y),
                                    yhat = round(mean(x$yhat), 10))))

  cuts <- df4$maxx[2:(nrow(df4) - 2)]
  return(list(df = manual_bin(data, yname, xname, cuts = cuts), 
              cuts = cuts))  
}
