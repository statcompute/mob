bads_bin <- function(data, y, x) {
# data: input dataframe
# y   : name of Y in the input dataframe
# x   : name of X in the input dataframe
#
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/manual_bin.R")
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/bads_bin.R")
# bads_bin(df, bad, ltv)
# $df
#  bin                           rule freq dist mv_cnt bad_freq bad_rate     woe     iv      ks
#   01                       $X <= 92 1789 0.31      0      225   0.1258 -0.5830 0.0868 14.8869
#   02             $X > 92 & $X <= 99 1003 0.17      0      206   0.2054  0.0030 0.0000 14.8359
#   03            $X > 99 & $X <= 104  974 0.17      0      212   0.2177  0.0766 0.0010 13.5290
#   04           $X > 104 & $X <= 111  846 0.14      0      198   0.2340  0.1703 0.0044 10.9363
#   05           $X > 111 & $X <= 120  670 0.11      0      188   0.2806  0.4144 0.0221  5.6029
#   06           $X > 120 | is.na($X)  555 0.10      1      167   0.3009  0.5129 0.0287  0.0000
# $cuts
# [1]  92  99 104 111 120

  ### GET THE DATA READY ###
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- data[!is.na(data[, xname]), c(yname, xname)]
  df2 <- df1[which(df1[[yname]] == 1), ]
  nbin <- round(1 / max(table(df2[[xname]]) / sum(table(df2[[xname]]))))

  ### A FUNCTION TO CUT A NUMERIC ARRAY INTO INTERVALS ###
  cut2 <- function(x, g) {
    c <- quantile(x, probs = seq(0, 1, 1 / g), na.rm = T, names = F, type = 3)
    return(c(0, rep(1, length(c) - 2), 0) + c)
  }

  ### BIN THE DATAFRAME ITERATIVELY UNTIL THE FIRST OCCURRENCE OF THE BREAK CONDITION ###
  repeat {
    cuts <- cut2(df2[[xname]], g = nbin)
    df1$cut <- cut(df1[[xname]], breaks = cuts, include.lowest = T)
    df3 <- Reduce(rbind, 
             Map(function(x) data.frame(xmean = mean(x[[xname]], na.rm = T), ymean = mean(x[[yname]])), 
               split(df1, df1$cut)))
    if(abs(cor(df3$xmean, df3$ymean, method = "spearman")) == 1 | nrow(df3) == 2) {
      break
    }
    nbin <- nbin - 1
  }

  return(list(df = manual_bin(data, yname, xname, cuts = cuts[c(-1, -length(cuts))]),
              cuts = cuts[c(-1, -length(cuts))]))
}
