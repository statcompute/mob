gbmcv_bin <- function(data, y, x) {
# INPUT 
# data: input dataframe
# y   : name of Y in the input dataframe with binary 0/1 values
# x   : name of X in the input dataframe with numeric values
# OUTPUT
# gbmcv_bin(df, bad, ltv)
# $df
#   bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    01                       $X <= 79  658 0.1127      0       61   0.0927 -0.9251 0.0718  7.7633
#    02             $X > 79 & $X <= 81   87 0.0149      0       10   0.1149 -0.6853 0.0056  8.5863
#   ...SKIPPED...
#    12           $X > 115 & $X <= 118  191 0.0327      0       57   0.2984  0.5012 0.0094  7.0754
#    13           $X > 118 | is.na($X)  677 0.1160      1      206   0.3043  0.5290 0.0374  0.0000
# $cuts
# [1]  79  81  83  90  94  95 100 101 110 112 115 118

  ### GET THINGS READY ###
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  df2 <- data.frame(y = df1[[yname]], x = df1[[xname]], x2 = df1[[xname]])

  ### DETECT THE CORRELATION DIRRECTION BETWEEN X AND Y ###
  cor <- cor(df2[, 2], df2[, 1], method = "spearman", use = "complete.obs")

  ### GET THE OUTPUT FROM A GENERALIZED BOOSTED REGRESSION MODEL ###
  set.seed(1)
  mdl <- gbm::gbm(y ~ x + x2, distribution = "bernoulli", data = df2, var.monotone = c(cor / abs(cor), cor / abs(cor)), 
                  bag.fraction = 1, n.minobsinnode = round(nrow(df2) / 10), cv.folds = 10)
  best.n <- gbm::gbm.perf(mdl, method = "cv", plot.it = FALSE)
  df3 <- data.frame(y = df2$y, x = df2$x, yhat = gbm::predict.gbm(mdl, n.trees = best.n, type = "response"))
  
  ### AGGREGATE THE GBM OUTPUT ###
  df4 <- Reduce(rbind, 
           lapply(split(df3, df3$yhat), 
             function(x) data.frame(maxx = max(x$x), 
                                    yavg = mean(x$y),
                                    yhat = round(mean(x$yhat), 8))))
  
  df5 <- df4[order(df4$maxx), ]
  h <- ifelse(df5[["yavg"]][1] %in% c(0, 1), 2, 1)
  t <- ifelse(df5[["yavg"]][nrow(df5)] %in% c(0, 1), 2, 1)
  cuts <- df5$maxx[h:max(h, (nrow(df5) - t))] 
  
  return(list(df = manual_bin(data, yname, xname, cuts = cuts), 
              cuts = cuts))  
}
