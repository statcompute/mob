pava_bin <- function(data, y, x) {
# data: input dataframe
# y   : name of Y in the input dataframe
# x   : name of X in the input dataframe
#
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/manual_bin.R")
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/pava_bin.R")
# pava_bin(df, bad, ltv)
# $df
#   bin                           rule freq dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    01                       $X <= 46   81 0.01      0        3   0.0370 -1.9021 0.0272  1.4298
#    02             $X > 46 & $X <= 71  312 0.05      0       28   0.0897 -0.9608 0.0363  5.2081
#    03             $X > 71 & $X <= 72   22 0.00      0        2   0.0909 -0.9466 0.0025  5.4718
#    04             $X > 72 & $X <= 73   27 0.00      0        3   0.1111 -0.7235 0.0019  5.7381
#    05             $X > 73 & $X <= 81  303 0.05      0       35   0.1155 -0.6797 0.0194  8.5863
#    06             $X > 81 & $X <= 83  139 0.02      0       17   0.1223 -0.6149 0.0074  9.7936
#    07             $X > 83 & $X <= 90  631 0.11      0       85   0.1347 -0.5040 0.0235 14.4513
#    08             $X > 90 & $X <= 94  529 0.09      0       89   0.1682 -0.2422 0.0049 16.4905
#    09             $X > 94 & $X <= 95  145 0.02      0       26   0.1793 -0.1651 0.0006 16.8807
#    10            $X > 95 & $X <= 100  907 0.16      0      198   0.2183  0.0804 0.0010 15.6024
#    11           $X > 100 & $X <= 101  195 0.03      0       44   0.2256  0.1229 0.0005 15.1771
#    12           $X > 101 & $X <= 110 1217 0.21      0      283   0.2325  0.1619 0.0057 11.6399
#    13           $X > 110 & $X <= 112  208 0.04      0       50   0.2404  0.2054 0.0016 10.8637
#    14           $X > 112 & $X <= 115  253 0.04      0       70   0.2767  0.3950 0.0075  8.9540
#    15           $X > 115 & $X <= 136  774 0.13      0      226   0.2920  0.4702 0.0333  1.8655
#    16           $X > 136 & $X <= 138   27 0.00      0        9   0.3333  0.6628 0.0024  1.5008
#    17           $X > 138 | is.na($X)   67 0.01      1       28   0.4179  1.0246 0.0154  0.0000
# $cuts
# [1]  46  71  72  73  81  83  90  94  95 100 101 110 112 115 136 138

  ### GET THINGS READY ###
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- data[!is.na(data[, xname]), c(xname, yname)]

  ### DETECT THE CORRELATION DIRRECTION BETWEEN X AND Y ###
  cor <- cor(df1[, 2], df1[, 1], method = "spearman", use = "complete.obs")

  ### GET THE OUTPUT FROM POOLED-ADJACENT-VIOLATORS ALGORITHM (PAVA) ###
  df2 <- with(isotone::gpava(df1[[xname]], cor / abs(cor) * df1[[yname]], ties = "secondary"), data.frame(x = z, y = y, yhat = x))

  ### AGGREGATE THE PAVA OUTPUT ###
  df3 <- Reduce(rbind, 
           lapply(split(df2, df2$yhat), 
             function(x) data.frame(minx = min(x$x), 
                                    maxx = max(x$x),
                                    nobs = nrow(x),
                                    yhat = round(mean(x$yhat), 4))))

  cuts <- df3$maxx[2:(nrow(df3) - 2)]
  return(list(df = manual_bin(data, yname, xname, cuts = cuts), 
              cuts = cuts))
}
