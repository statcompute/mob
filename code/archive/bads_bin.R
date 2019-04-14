bads_bin <- function(data, y, x) {
# INPUT
# data: input dataframe
# y   : name of Y in the input dataframe with 0/1 binary values
# x   : name of X in the input dataframe with numeric values
# OUTPUT
# bads_bin(df, bad, ltv)
# $df
#   bin                           rule freq   dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    01                       $X <= 91 1651 0.2829      0      201   0.1217 -0.6201 0.0895 14.4372
#    02             $X > 91 & $X <= 98  989 0.1694      0      199   0.2012 -0.0228 0.0001 14.8206
#   ...SKIPPED...
#    06           $X > 113 & $X <= 121  526 0.0901      0      153   0.2909  0.4648 0.0221  5.2943
#    07           $X > 121 | is.na($X)  501 0.0858      1      153   0.3054  0.5342 0.0283  0.0000
# $cuts
# [1]  91  98 102 107 113 121

  ### GET THE DATA READY ###
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  df2 <- subset(df1, df1[[yname]] == 1)
  nbin <- round(1 / max(table(df2[[xname]]) / length(df2[[xname]])))

  ### BIN THE DATAFRAME ITERATIVELY UNTIL THE FIRST OCCURRENCE OF THE BREAK CONDITION ###
  repeat {
    cuts <- Hmisc::cut2(df2[[xname]], g = nbin, onlycuts = T)
    df1$cut <- cut(df1[[xname]], breaks = cuts, include.lowest = T)
    df3 <- Reduce(rbind, 
             Map(function(x) data.frame(xmean = mean(x[[xname]]), 
                                        ymean = mean(x[[yname]])), 
               split(df1, df1$cut)))

    if(round(abs(cor(df3$xmean, df3$ymean, method = "spearman")), 8) == 1) {
      if(max(df3$ymean) < 1 & min(df3$ymean) > 0) {
        break
      }
    }
    nbin <- nbin - 1
  }

  return(list(df   = manual_bin(data, yname, xname, cuts = cuts[c(-1, -length(cuts))]),
              cuts = cuts[c(-1, -length(cuts))]))
}
