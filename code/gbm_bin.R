gbm_bin <- function(data, y, x) {
# INPUT   
# data: input dataframe
# y   : name of Y in the input dataframe with binary 0/1 values
# x   : name of X in the input dataframe with numeric values
# OUTPUT
# gbm_bin(df, bad, ltv)
# $df
#   bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    01                       $X <= 71  393 0.0673      0       31   0.0789 -1.1017 0.0574  5.2081
#    02             $X > 71 & $X <= 72   22 0.0038      0        2   0.0909 -0.9466 0.0025  5.4718
#   ...SKIPPED...
#    14           $X > 115 & $X <= 136  774 0.1326      0      226   0.2920  0.4702 0.0333  1.8655
#    15           $X > 136 | is.na($X)   94 0.0161      1       37   0.3936  0.9238 0.0172  0.0000
# $cuts
# [1]  71  72  73  81  83  90  94  95 100 101 110 112 115 136

  ### GET THINGS READY ###
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  df2 <- data.frame(y = df1[[yname]], x = df1[[xname]], x2 = df1[[xname]])

  ### DETECT THE CORRELATION DIRRECTION BETWEEN X AND Y ###
  cor <- cor(df2[, 2], df2[, 1], method = "spearman", use = "complete.obs")

  ### GET THE OUTPUT FROM A GENERALIZED BOOSTED REGRESSION MODEL ###
  mdl <- gbm::gbm(y ~ x + x2, distribution = "bernoulli", data = df2, var.monotone = c(cor / abs(cor), cor / abs(cor)), 
                  bag.fraction = 1, n.minobsinnode = round(nrow(df2) / 100))
  df3 <- data.frame(y = df2$y, x = df2$x, yhat = gbm::predict.gbm(mdl, n.trees = mdl$n.trees, type = "response"))
  
  ### AGGREGATE THE GBM OUTPUT ###
  df4 <- Reduce(rbind, 
           lapply(split(df3, df3$yhat), 
             function(x) data.frame(maxx = max(x$x), 
                                    yavg = mean(x$y),
                                    yhat = round(mean(x$yhat), 8))))
  
  cuts <- sort(df4$maxx[2:max(2, (nrow(df4) - 2))])
  return(list(df = manual_bin(data, yname, xname, cuts = cuts), 
              cuts = cuts))  
}
