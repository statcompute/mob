### import an utility function manual_bin() ###
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/manual_bin.R")

bump_bin <- function(data, y, x, n) {
  ### SET THE MINIMUM REQUIREMENT FOR THE BINNING SIZE ###  
  n1 <- 50
  n2 <- 10
  set.seed(2019)
  seeds <- c(0, round(runif(n) * as.numeric(paste('1e', ceiling(log10(n)) + 2, sep = '')), 0))
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- data[, c(yname, xname)]
  df2 <- df1[!is.na(df1[, xname]), c(xname, yname)]
  cor <- cor(df2[, 2], df2[, 1], method = "spearman", use = "complete.obs")
  ### MONOTONIC BINNING WITH BOOTSTRAP SAMPLES ###
  bin <- function(seed) {
    if (seed == 0) {
      smp <- df2
    }
    else {
      set.seed(seed)
      smp <- df2[sample(seq(nrow(df2)), nrow(df2), replace = T), ]
    }
    reg <- isoreg(smp[, 1], cor / abs(cor) * smp[, 2])
    cut <- knots(as.stepfun(reg))
    df2$cut <- cut(df2[[xname]], breaks = unique(cut), include.lowest = T)
    df3 <- Reduce(rbind,
             lapply(split(df2, df2$cut),
               function(x) data.frame(n = nrow(x), b = sum(x[[yname]]), g = sum(1 - x[[yname]]),
                                      maxx = max(x[[xname]]), minx = min(x[[xname]]))))
    df4 <- df3[which(df3[["n"]] > n1 & df3[["b"]] > n2 & df3[["g"]] > n2), ]
    out <- manual_bin(df2, yname, xname, cuts = df4$maxx[-nrow(df4)])
    return(data.frame(iv = sum(out$InfoValue), nbin = nrow(out),
                      cuts = I(list(df4$maxx[-nrow(df4)])),
                      abs_cor = abs(cor(as.numeric(row.names(out)[1:nrow(out)]),
                                        out$WoE[1:nrow(out)], method = "spearman"))))
  }
  bump_out <- Reduce(rbind, parallel::mclapply(seeds, mc.cores = parallel::detectCores(), bin))
  ### FIND THE CUT MAXIMIZING THE INFORMATION VALUE ###
  cut2 <- bump_out[order(-bump_out["abs_cor"], -bump_out["iv"], bump_out["nbin"]), ]$cuts[[1]]
  return(manual_bin(df1, yname, xname, cuts = cut2))
}

# bump_bin(df, bad, ltv, n = 500)
#Bin                           Rule Frequency Percent MV_Cnt Bad_Freq Bad_Rate     WoE InfoValue KS_Stat
# 01                       $X <= 72       415    0.07      0       33   0.0795 -1.0930    0.0598    5.47
# 02             $X > 72 & $X <= 83       469    0.08      0       55   0.1173 -0.6626    0.0286    9.79
# 03             $X > 83 & $X <= 90       631    0.11      0       85   0.1347 -0.5040    0.0235   14.45
# 04             $X > 90 & $X <= 95       674    0.12      0      115   0.1706 -0.2253    0.0055   16.88
# 05            $X > 95 & $X <= 100       907    0.16      0      198   0.2183  0.0804    0.0010   15.60
# 06           $X > 100 & $X <= 110      1412    0.24      0      327   0.2316  0.1566    0.0062   11.64
# 07           $X > 110 & $X <= 112       208    0.04      0       50   0.2404  0.2054    0.0016   10.86
# 08           $X > 112 & $X <= 115       253    0.04      0       70   0.2767  0.3950    0.0075    8.95
# 09           $X > 115 & $X <= 134       748    0.13      0      219   0.2928  0.4740    0.0328    2.04
# 10           $X > 134 & $X <= 138        53    0.01      0       16   0.3019  0.5176    0.0028    1.50
# 11           $X > 138 | is.na($X)        67    0.01      1       28   0.4179  1.0246    0.0154    0.00
