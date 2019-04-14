qtl_bin <- function(data, y, x) {
# INPUT
# data: input dataframe
# y   : name of Y in the input dataframe with 0/1 binary values
# x   : name of X in the input dataframe with numeric values
# OUTPUT
# inc_bin(df, bad, ltv)
# $df
#   bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    01                       $X <= 84  956 0.1638      0      102   0.1067 -0.7690 0.0759  9.8728
#   ...SKIPPED...
#    07           $X > 117 | is.na($X)  729 0.1249      1      218   0.2990  0.5041 0.0364  0.0000

# $cuts
# [1]  84  93  99 103 109 117

  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))  
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  nbin <- 2
 
  repeat {
    pts <- Hmisc::cut2(df1[[xname]], g = nbin + 1, onlycuts = T)
    df1$cut <- cut(df1[[xname]], breaks = pts, include.lowest = T)
    df2 <- Reduce(rbind, 
             Map(function(x) data.frame(xmean = mean(x[[xname]]), 
                                        ymean = mean(x[[yname]])), 
               split(df1, df1$cut)))

    flg1 <- ifelse(round(abs(cor(df2$xmean, df2$ymean, method = "spearman", use = "complete.obs")), 8) < 1, 1, 0)
    flg2 <- ifelse(max(df2$ymean) == 1 | min(df2$ymean) == 0, 1, 0)

    if((flg1 + flg2) > 0) {
      cuts <- Hmisc::cut2(df1[[xname]], g = nbin, onlycuts = T)
      break
    }
    nbin <- nbin + 1
  }

  return(list(df   = manual_bin(data, yname, xname, cuts = cuts[2:(length(cuts) - 1)]), 
              cuts = cuts[2:(length(cuts) - 1)]))
}
