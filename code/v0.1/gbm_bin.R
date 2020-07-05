gbm_bin <- function(data, y, x) {
# INPUT   
# data: input dataframe
# y   : name of Y in the input dataframe with binary 0/1 values
# x   : name of X in the input dataframe with numeric values
# OUTPUT
# gbm_bin(df, bad, ltv)
# $df
#   bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    01                       $X <= 46   81 0.0139      0        3   0.0370 -1.9021 0.0272  1.4298
#   ...SKIPPED...
#    17           $X > 138 | is.na($X)   67 0.0115      1       28   0.4179  1.0246 0.0154  0.0000
# $cuts
# [1]  46  71  72  73  81  83  90  94  95 100 101 110 112 115 136 138

  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  df2 <- data.frame(y = df1[[yname]], x = df1[[xname]], x2 = df1[[xname]])
  spcor <- cor(df2[, 2], df2[, 1], method = "spearman", use = "complete.obs")

  mdl <- gbm::gbm(y ~ x + x2, distribution = "bernoulli", data = df2, var.monotone = c(spcor / abs(spcor), spcor / abs(spcor)), 
                  bag.fraction = 1, n.minobsinnode = round(nrow(df2) / 100))
  df3 <- data.frame(y = df2$y, x = df2$x, yhat = gbm::predict.gbm(mdl, n.trees = mdl$n.trees, type = "response"))
  
  df4 <- Reduce(rbind, 
           lapply(split(df3, df3$yhat), 
             function(x) data.frame(maxx = max(x$x), 
                                    yavg = mean(x$y),
                                    yhat = round(mean(x$yhat), 8))))

  df5 <- df4[order(df4$maxx), ]
  h <- ifelse(df5[["yavg"]][1] %in% c(0, 1), 2, 1)
  t <- ifelse(df5[["yavg"]][nrow(df5)] %in% c(0, 1), 2, 1)
  cuts <- df5$maxx[h:max(h, (nrow(df5) - t))]

  return(list(df   = manual_bin(data, yname, xname, cuts = cuts), 
              cuts = cuts))  
}
