### import an utility function manual_bin() ###
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/manual_bin.R")

isoreg_bin <- function(data, y, x) {
  ### SET THE MINIMUM REQUIREMENT FOR THE BINNING SIZE ###
  n1 <- 50
  n2 <- 10
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- data[, c(yname, xname)]
  df2 <- df1[!is.na(df1[, xname]), c(xname, yname)]
  cor <- cor(df2[, 2], df2[, 1], method = "spearman", use = "complete.obs")
  reg <- isoreg(df2[, 1], cor / abs(cor) * df2[, 2])
  cut <- knots(as.stepfun(reg))
  df2$cut <- cut(df2[[xname]], breaks = unique(cut), include.lowest = T)
  df3 <- Reduce(rbind,
           lapply(split(df2, df2$cut),
             function(x) data.frame(n = nrow(x), b = sum(x[[yname]]), g = sum(1 - x[[yname]]),
                                    maxx = max(x[[xname]]), minx = min(x[[xname]]))))
  df4 <- df3[which(df3[["n"]] > n1 & df3[["b"]] > n2 & df3[["g"]] > n2), ]
  return(manual_bin(df1, yname, xname, cuts = df4$maxx[-nrow(df4)]))
}

# isoreg_bin(df, bad, ltv)
#Bin                           Rule Frequency Percent MV_Cnt Bad_Freq Bad_Rate     WoE InfoValue KS_Stat
# 01                       $X <= 71       393    0.07      0       31   0.0789 -1.1017    0.0574    5.21
# 02             $X > 71 & $X <= 81       352    0.06      0       40   0.1136 -0.6982    0.0236    8.59
# 03             $X > 81 & $X <= 83       139    0.02      0       17   0.1223 -0.6149    0.0074    9.79
# 04             $X > 83 & $X <= 90       631    0.11      0       85   0.1347 -0.5040    0.0235   14.45
# 05             $X > 90 & $X <= 94       529    0.09      0       89   0.1682 -0.2422    0.0049   16.49
# 06             $X > 94 & $X <= 95       145    0.02      0       26   0.1793 -0.1651    0.0006   16.88
# 07            $X > 95 & $X <= 100       907    0.16      0      198   0.2183  0.0804    0.0010   15.60
# 08           $X > 100 & $X <= 101       195    0.03      0       44   0.2256  0.1229    0.0005   15.18
# 09           $X > 101 & $X <= 110      1217    0.21      0      283   0.2325  0.1619    0.0057   11.64
# 10           $X > 110 & $X <= 112       208    0.04      0       50   0.2404  0.2054    0.0016   10.86
# 11           $X > 112 & $X <= 115       253    0.04      0       70   0.2767  0.3950    0.0075    8.95
# 12           $X > 115 & $X <= 136       774    0.13      0      226   0.2920  0.4702    0.0333    1.87
# 13           $X > 136 | is.na($X)        94    0.02      1       37   0.3936  0.9238    0.0172    0.00
