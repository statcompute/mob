wgbm_bin <- function(data, y, x, w) {
# input   
# data : input dataframe
# y    : name of Y in the input dataframe with binary 0/1 values
# x    : name of X in the input dataframe with numeric values
# w    : name of weight in the input dataframe with positive numeric values

  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  wname <- deparse(substitute(w))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[wname]] > 0 & data[[yname]] %in% c(0, 1), 
                select = c(xname, yname, wname))
  df2 <- data.frame(y = df1[[yname]], x = df1[[xname]], x2 = df1[[xname]], w = df1[[wname]])

  spcor <- cor(df2$x, df2$y, method = "spearman", use = "complete.obs")
  
  mdl <- gbm::gbm(y ~ x + x2, distribution = "bernoulli", data = df2, var.monotone = c(spcor / abs(spcor), spcor / abs(spcor)), 
                  bag.fraction = 1, n.minobsinnode = round(nrow(df2) / 100), weights = w)
 
  df3 <- data.frame(y = df2$y, x = df2$x, w = df2$w, yhat = gbm::predict.gbm(mdl, n.trees = mdl$n.trees, type = "response"))
  
  df4 <- Reduce(rbind, 
           lapply(split(df3, df3$yhat), 
             function(x_) data.frame(maxx = max(x_$x), 
                                     yavg = weighted.mean(x_$y, x_$w),
                                     yhat = round(mean(x_$yhat), 8))))

  df5 <- df4[order(df4$maxx), ]
  h <- ifelse(df5[["yavg"]][1] %in% c(0, 1), 2, 1)
  t <- ifelse(df5[["yavg"]][nrow(df5)] %in% c(0, 1), 2, 1)
  cuts <- df5$maxx[h:max(h, (nrow(df5) - t))]

  bin <- manual_bin(data, yname, xname, cuts = cuts)
  woe <- cal_woe(cbind(idx_ = seq(nrow(data)), data), xname, bin)

  df6 <- Reduce(rbind, 
           Map(function(x_) data.frame(bin      = x_[1, "woe.bin"],
                                       rule     = subset(bin, bin == x_[1, "woe.bin"])[["rule"]],
                                       cnt      = nrow(x_),
                                       freq     = round(sum(x_[[wname]]), 2),
                                       dist     = round(sum(x_[[wname]]) / sum(woe$df[[wname]]), 4),
                                       mv_wt    = round(sum(subset(x_, is.na(x_[[xname]]))[[wname]]), 2),
                                       bad_freq = round(sum(subset(x_, x_[[yname]] == 1)[[wname]]), 2)),
             split(woe$df, woe$df[["woe.bin"]])))

  df6$bad_rate <- round(df6$bad_freq / df6$freq, 4)
  df6$woe <- with(df6, round(log((bad_freq / sum(bad_freq)) / ((freq - bad_freq) / (sum(freq - bad_freq)))), 4))
  df6$iv <- with(df6, round((bad_freq / sum(bad_freq) - (freq - bad_freq) / sum(freq - bad_freq)) * woe, 4))
  df6$ks <- with(df6, round(abs(cumsum(bad_freq) / sum(bad_freq) - cumsum(freq - bad_freq) / sum(freq - bad_freq)) * 100, 4))
  return(list(df = df6, cuts = cuts))
}
