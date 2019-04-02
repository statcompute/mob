gbmcv_bin <- function(data, y, x) {
### PARAMETERS ### 
# data: input dataframe
# y   : name of Y in the input dataframe
# x   : name of X in the input dataframe
### HOW TO USE ###
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/manual_bin.R")
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/gbmcv_bin.R")
# gbmcv_bin(df, bad, tot_derog)
### OUTPUT ###
# $df
#   bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    00                      is.na($X)  213 0.0365    213       70   0.3286  0.6416 0.0178  2.7716
#    01                        $X <= 1 3741 0.6409      0      560   0.1497 -0.3811 0.0828 18.9469
#    02               $X > 1 & $X <= 2  478 0.0819      0      121   0.2531  0.2740 0.0066 16.5222
#    03                         $X > 2 1405 0.2407      0      445   0.3167  0.5871 0.0970  0.0000
# $cuts
# [1] 1 2

  ### GET THINGS READY ###
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- data[!is.na(data[, xname]), c(xname, yname)]
  df2 <- data.frame(y = df1[[yname]], x = df1[[xname]], x2 = df1[[xname]])

  ### DETECT THE CORRELATION DIRRECTION BETWEEN X AND Y ###
  cor <- cor(df2[, 2], df2[, 1], method = "spearman", use = "complete.obs")

  ### GET THE OUTPUT FROM A GENERALIZED BOOSTED REGRESSION MODEL ###
  set.seed(1)
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

  cuts <- df4$maxx[2:max(2, (nrow(df4) - 2))]
  return(list(df = manual_bin(data, yname, xname, cuts = cuts), 
              cuts = cuts))  
}
