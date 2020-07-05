wqtl_bin <- function(data, y, x, w) {
# input
# data: input dataframe
# y   : name of Y in the input dataframe with 0/1 binary values
# x   : name of X in the input dataframe with numeric values
# w   : name of weight in the input dataframe with positive numeric values

  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))  
  wname <- deparse(substitute(w))  
  df1 <- subset(data, !is.na(data[[xname]]) & data[[wname]] > 0 & data[[yname]] %in% c(0, 1), 
                select = c(xname, yname, wname))
  nbin <- 2
 
  repeat {
    pts <- Hmisc::cut2(df1[[xname]], g = nbin + 1, onlycuts = T)
    df1$cut <- cut(df1[[xname]], breaks = pts, include.lowest = T)
    df2 <- Reduce(rbind, 
             Map(function(x_) data.frame(xmean = mean(x_[[xname]]), 
                                         wmean = weighted.mean(x_[[yname]], x_[[wname]]),
                                         ymean = mean(x_[[yname]])), 
               split(df1, df1$cut)))

    flg1 <- ifelse(round(abs(cor(df2$xmean, df2$wmean, method = "spearman", use = "complete.obs")), 8) < 1, 1, 0)
    flg2 <- ifelse(max(df2$wmean) == 1 | min(df2$wmean) == 0, 1, 0)

    if((flg1 + flg2) > 0) {
      cuts <- Hmisc::cut2(df1[[xname]], g = nbin, onlycuts = T)
      break
    }
    nbin <- nbin + 1
  }

  bin <- manual_bin(data, yname, xname, cuts = cuts[2:(length(cuts) - 1)])
  woe <- cal_woe(cbind(idx_ = seq(nrow(data)), data), xname, bin)
 
  df3 <- Reduce(rbind, 
           Map(function(x_) data.frame(bin      = x_[1, "woe.bin"],
                                       rule     = subset(bin, bin == x_[1, "woe.bin"])[["rule"]],
                                       cnt      = nrow(x_),
                                       freq     = round(sum(x_[[wname]]), 2),
                                       dist     = round(sum(x_[[wname]]) / sum(woe$df[[wname]]), 4),
                                       mv_wt    = round(sum(subset(x_, is.na(x_[[xname]]))[[wname]]), 2),
                                       bad_freq = round(sum(subset(x_, x_[[yname]] == 1)[[wname]]), 2)),
             split(woe$df, woe$df[["woe.bin"]])))

  df3$bad_rate <- round(df3$bad_freq / df3$freq, 4)
  df3$woe <- with(df3, round(log((bad_freq / sum(bad_freq)) / ((freq - bad_freq) / (sum(freq - bad_freq)))), 4))
  df3$iv <- with(df3, round((bad_freq / sum(bad_freq) - (freq - bad_freq) / sum(freq - bad_freq)) * woe, 4))
  df3$ks <- with(df3, round(abs(cumsum(bad_freq) / sum(bad_freq) - cumsum(freq - bad_freq) / sum(freq - bad_freq)) * 100, 4))
  return(list(df = df3, cuts = cuts[2:(length(cuts) - 1)]))
}
