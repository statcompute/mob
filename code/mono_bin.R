mono_bin <- function(data, y, x) {
# data: input dataframe
# y   : name of Y in the input dataframe
# x   : name of X in the input dataframe
# 
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/manual_bin.R")  
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/mono_bin.R")
# mono_bin(df, bad, ltv)
# $df
# bin                           rule freq dist mv_cnt bad_freq bad_rate     woe     iv      ks
#  01                       $X <= 86 1108 0.19      0      122   0.1101 -0.7337 0.0810 11.0448
#  02             $X > 86 & $X <= 95 1081 0.19      0      166   0.1536 -0.3510 0.0205 16.8807
#  03            $X > 95 & $X <= 101 1102 0.19      0      242   0.2196  0.0880 0.0015 15.1771
#  04           $X > 101 & $X <= 106  743 0.13      0      177   0.2382  0.1935 0.0050 12.5734
#  05           $X > 106 & $X <= 115  935 0.16      0      226   0.2417  0.2126 0.0077  8.9540
#  06           $X > 115 | is.na($X)  868 0.15      1      263   0.3030  0.5229 0.0468  0.0000
# $cuts
# [1]  86  95 101 106 115

  ### GET THE DATA READY ###
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))  
  df1 <- data[!is.na(data[, xname]), c(xname, yname)]  
  nbin <- min(100, nrow(unique(df1[[xname]])))

  ### A FUNCTION TO CUT A NUMERIC ARRAY INTO INTERVALS ###
  cut2 <- function(x, g) {
    c <- unique(quantile(x, probs = seq(0, 1, 1 / g), na.rm = T, names = F, type = 3))
    return(c(0, rep(1, length(c) - 2), 0) + c)
  }
 
  ### BIN THE DATAFRAME ITERATIVELY UNTIL THE FIRST OCCURRENCE OF THE BREAK CONDITION ###
  repeat {
    cuts <- cut2(df1[[xname]], nbin)
    df1$cut <- cut(df1[[xname]], breaks = cuts, include.lowest = T)    
    df2 <- Reduce(rbind, 
             Map(function(x) data.frame(xmean = mean(x[[xname]], na.rm = T), 
                                        ymean = mean(x[[yname]], na.rm = T)), 
               split(df1, df1$cut)))
    if(abs(cor(df2$xmean, df2$ymean, method = "spearman")) == 1 | nrow(df2) == 2) {
      break
    }    
    nbin <- nbin - 1
  }

  return(list(df   = manual_bin(data, yname, xname, cuts = cuts[c(-1, -length(cuts))]), 
              cuts = cuts[c(-1, -length(cuts))]))
}
