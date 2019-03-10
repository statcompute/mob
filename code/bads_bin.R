### import an utility function manual_bin() ###
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/manual_bin.R")

bads_bin <- function(data, y, x) {
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- data[!is.na(data[, xname]), c(yname, xname)]
  df2 <- df1[which(df1[[yname]] == 1), ]
  nbin <- round(1 / max(table(df2[[xname]]) / sum(table(df2[[xname]]))))
  cut2 <- function(x, g) {
    c <- unique(quantile(x, probs = seq(0, 1, 1 / g), na.rm = T, names = F, type = 3))
    return(c(0, rep(1, length(c) - 2), 0) + c)
  }
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
  return(manual_bin(data, yname, xname, cuts = cuts[c(-1, -length(cuts))]))
}

# bads_bin(df, bad, ltv)
#Bin                           Rule Frequency Percent MV_Cnt Bad_Freq Bad_Rate     WoE InfoValue KS_Stat
# 01                       $X <= 92      1789    0.31      0      225   0.1258 -0.5830    0.0868   14.89
# 02             $X > 92 & $X <= 99      1003    0.17      0      206   0.2054  0.0030    0.0000   14.84
# 03            $X > 99 & $X <= 104       974    0.17      0      212   0.2177  0.0766    0.0010   13.53
# 04           $X > 104 & $X <= 111       846    0.14      0      198   0.2340  0.1703    0.0044   10.94
# 05           $X > 111 & $X <= 120       670    0.11      0      188   0.2806  0.4144    0.0221    5.60
# 06           $X > 120 | is.na($X)       555    0.10      1      167   0.3009  0.5129    0.0287    0.00
