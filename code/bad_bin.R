bad_bin <- function(data, y, x) {
# INPUT
# data: input dataframe
# y   : name of Y in the input dataframe with 0/1 binary values
# x   : name of X in the input dataframe with numeric values
# OUTPUT
# bads_bin(df, bad, ltv)
# $df
#   bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    01                       $X <= 91 1651 0.2829      0      201   0.1217 -0.6201 0.0895 14.4372
#   ...SKIPPED...
#    07           $X > 121 | is.na($X)  501 0.0858      1      153   0.3054  0.5342 0.0283  0.0000
# $cuts
# [1]  91  98 102 107 113 121

  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  df2 <- subset(df1, df1[[yname]] == 1)
  nbin <- 2

  repeat {
    pts <- Hmisc::cut2(df2[[xname]], g = nbin + 1, onlycuts = T)
    df1$cut <- cut(df1[[xname]], breaks = pts, include.lowest = T)
    df3 <- Reduce(rbind, 
             Map(function(x) data.frame(xmean = mean(x[[xname]]), 
			                ymean = mean(x[[yname]])), 
               split(df1, df1$cut)))

    flg1 <- ifelse(round(abs(cor(df3$xmean, df3$ymean, method = "spearman", use = "complete.obs")), 8) < 1, 1, 0)
    flg2 <- ifelse(max(df3$ymean) == 1 | min(df3$ymean) == 0, 1, 0)

    if((flg1 + flg2) > 0) {
      cuts <- Hmisc::cut2(df2[[xname]], g = nbin, onlycuts = T)
      break
    }
    nbin <- nbin + 1
  }

  return(list(df   = manual_bin(data, yname, xname, cuts = cuts[2:(length(cuts) - 1)]),
              cuts = cuts[2:(length(cuts) - 1)]))
}
