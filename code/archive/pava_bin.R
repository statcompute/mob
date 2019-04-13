pava_bin <- function(data, y, x) {
# INPUT
# data: input dataframe
# y   : name of Y in the input dataframe with binary 0/1 values
# x   : name of X in the input dataframe with numeric values
# OUTPUT
# pava_bin(df, bad, ltv)
# $df
#   bin                           rule freq dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    01                       $X <= 46   81 0.01      0        3   0.0370 -1.9021 0.0272  1.4298
#    02             $X > 46 & $X <= 71  312 0.05      0       28   0.0897 -0.9608 0.0363  5.2081
#   ...SKIPPED...
#    16           $X > 136 & $X <= 138   27 0.00      0        9   0.3333  0.6628 0.0024  1.5008
#    17           $X > 138 | is.na($X)   67 0.01      1       28   0.4179  1.0246 0.0154  0.0000
# $cuts
# [1]  46  71  72  73  81  83  90  94  95 100 101 110 112 115 136 138

  ### GET THINGS READY ###
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))

  ### DETECT THE CORRELATION DIRRECTION BETWEEN X AND Y ###
  cor <- cor(df1[, 2], df1[, 1], method = "spearman", use = "complete.obs")

  ### GET THE OUTPUT FROM POOLED-ADJACENT-VIOLATORS ALGORITHM (PAVA) ###
  df2 <- with(isotone::gpava(df1[[xname]], cor / abs(cor) * df1[[yname]], ties = "secondary"), data.frame(x = z, y = y, yhat = x))

  ### AGGREGATE THE PAVA OUTPUT ###
  df3 <- Reduce(rbind,
           lapply(split(df2, df2$yhat),
             function(x) data.frame(maxx = max(x$x),
                                    yavg = abs(mean(x$y)),
                                    yhat = abs(round(mean(x$yhat), 8)))))

  df4 <- df3[order(df3$maxx), ]  
  h <- ifelse(abs(df4[["yavg"]][1]) %in% c(0, 1), 2, 1)
  t <- ifelse(abs(df4[["yavg"]][nrow(df4)]) %in% c(0, 1), 2, 1)
  cuts <- df4$maxx[h:(nrow(df4) - t)] 

  return(list(df = manual_bin(data, yname, xname, cuts = cuts), 
              cuts = cuts))
}
