###############################################################################
# PACKAGE NAME: MOB (MONOTONIC OPTIMAL BINNING)
# AUTHOR      : WENSUI LIU
# DISCLAIMER  : THIS IS MY WEEKEND PROJECT AND NOT RELATED TO MY CURRENT WORK WITH MY EMPLOYER
#               IT IS FREE (AS FREE BEER) TO USE AND DISTRIBUTE
###############################################################################

manual_bin <- function(df, yname, xname, cuts) {
# INPUT
# df   : input dataframe
# yname: a string of Y in the dataframe
# xname: a string of X in the dataframe
# cuts : a numeric array

  cuts <- sort(c(-Inf, cuts, Inf))
  df1 <- df[which(df[[yname]] %in% c(0, 1)), c(yname, xname)]
  all_cnt <- nrow(df1)
  all_bcnt <- sum(df1[[yname]])

  if (all(!is.na(df1[[xname]])) == TRUE) {
    miss_flg <- 0
    df2 <- df1
  }
  else {
    miss_flg <- 1
    df2 <- df1[!is.na(df1[, xname]), ]
    mis <- df1[is.na(df1[, xname]), ]
    mis_cnt <- nrow(mis)
    mis_bcnt <- sum(mis[[yname]])
    if (sum(mis[[yname]]) %in% c(nrow(mis), 0)) {
      miss_flg <- 2
    }
  }

  for (i in seq(length(cuts) - 1)) {
    bin <- sprintf("%02d", i)
    bin_cnt <- nrow(df2[which(df2[[xname]] > cuts[i] & df2[[xname]] <= cuts[i + 1]), ])
    bin_bcnt <- nrow(df2[which(df2[[xname]] > cuts[i] & df2[[xname]] <= cuts[i + 1] & df2[[yname]] == 1), ])
    if (i == 1) {
      bin_summ <- data.frame(bin = bin, xmin = cuts[i], xmax = cuts[i + 1], cnt = bin_cnt, bcnt = bin_bcnt)
    }
    else {
      bin_summ <- rbind(bin_summ,
                        data.frame(bin = bin, xmin = cuts[i], xmax = cuts[i + 1], cnt = bin_cnt, bcnt = bin_bcnt))
    }
  }

  bin_summ$mis_cnt <- 0
  if (miss_flg == 1) {
    bin_summ <- rbind(data.frame(bin = sprintf("%02d", 0), xmin = NA, xmax = NA, cnt = mis_cnt, bcnt = mis_bcnt, mis_cnt = mis_cnt),
                      bin_summ)
  }
  if (miss_flg == 2) {
    rate <- bin_summ$bcnt / bin_summ$cnt
    if (mis_bcnt == 0) {
      bin_summ[rate == min(rate), "cnt"] <- bin_summ[rate == min(rate), "cnt"] + mis_cnt
      bin_summ[rate == min(rate), "mis_cnt"] <- mis_cnt
    }
    else {
      bin_summ[rate == max(rate), "cnt"] <- bin_summ[rate == max(rate), "cnt"] + mis_cnt
      bin_summ[rate == max(rate), "bcnt"] <- bin_summ[rate == max(rate), "bcnt"] + mis_bcnt
      bin_summ[rate == max(rate), "mis_cnt"] <- mis_cnt
    }
  }

  bin_summ$dist <- bin_summ$cnt / all_cnt
  bin_summ$brate <- bin_summ$bcnt / bin_summ$cnt
  bin_summ$woe <- log((bin_summ$bcnt / all_bcnt) / ((bin_summ$cnt - bin_summ$bcnt) / (all_cnt - all_bcnt)))
  bin_summ$iv <- (bin_summ$bcnt / all_bcnt - (bin_summ$cnt - bin_summ$bcnt) / (all_cnt - all_bcnt)) * bin_summ$woe
  bin_summ$ks <- abs(cumsum(bin_summ$bcnt) / all_bcnt - cumsum(bin_summ$cnt - bin_summ$bcnt) / (all_cnt - all_bcnt)) * 100
  bin_summ$rule <- NA

  for (i in seq(nrow(bin_summ))) {
    if (bin_summ[i, ]$bin == '00') {
      bin_summ[i, ]$rule <- paste("is.na($X)", sep = '')
    }
    else if (bin_summ[i, ]$bin == '01') {
      if (bin_summ[i, ]$mis_cnt > 0) {
        bin_summ[i, ]$rule <- paste("$X <= ", bin_summ[i, ]$xmax, " | is.na($X)", sep = '')
      }
      else {
        bin_summ[i, ]$rule <- paste("$X <= ", bin_summ[i, ]$xmax, sep = '')
      }
    }
    else if (i == nrow(bin_summ)) {
      if (bin_summ[i, ]$mis_cnt > 0) {
        bin_summ[i, ]$rule <- paste("$X > ", bin_summ[i, ]$xmin, " | is.na($X)", sep = '')
      }
      else {
        bin_summ[i, ]$rule <- paste("$X > ", bin_summ[i, ]$xmin, sep = '')
      }
    }
    else {
      bin_summ[i, ]$rule <- paste("$X > ", bin_summ[i, ]$xmin, " & ", "$X <= ", bin_summ[i, ]$xmax, sep = '')
    }
  }
  
  return(data.frame(bin      = format(bin_summ$bin, width = 5, justify = "right"),
                    rule     = format(bin_summ$rule, width = 30, justify = "right"),
                    freq     = bin_summ$cnt,
                    dist     = round(bin_summ$dist, 4),
                    mv_cnt   = bin_summ$mis_cnt,  
                    bad_freq = bin_summ$bcnt,
                    bad_rate = round(bin_summ$brate, 4),
                    woe      = round(bin_summ$woe, 4), 
                    iv       = round(bin_summ$iv, 4), 
                    ks       = round(bin_summ$ks, 4)))
}

###############################################################################

qtl_bin <- function(data, y, x) {
# INPUT
# data: input dataframe
# y   : name of Y in the input dataframe with 0/1 binary values
# x   : name of X in the input dataframe with numeric values

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

###############################################################################

bad_bin <- function(data, y, x) {
# INPUT
# data: input dataframe
# y   : name of Y in the input dataframe with 0/1 binary values
# x   : name of X in the input dataframe with numeric values

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

###############################################################################

iso_bin <- function(data, y, x) {
# INPUT	
# data: input dataframe
# y   : name of Y in the input dataframe with binary 0/1 values
# x   : name of X in the input dataframe with numeric values

  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  df2 <- df1[order(df1[[xname]]), ]
  spcor <- cor(df2[, 2], df2[, 1], method = "spearman", use = "complete.obs")

  df3 <- with(isoreg(df2[[xname]], spcor / abs(spcor) * df2[[yname]]), data.frame(x = x, y = y, yhat = yf))

  df4 <- Reduce(rbind, 
           lapply(split(df3, df3$yhat), 
             function(x) data.frame(maxx = max(x$x), 
                                    yavg = abs(mean(x$y)),
                                    yhat = abs(round(mean(x$yhat), 8)))))

  df5 <- df4[order(df4$maxx), ]  
  h <- ifelse(df5[["yavg"]][1] %in% c(0, 1), 2, 1)
  t <- ifelse(df5[["yavg"]][nrow(df5)] %in% c(0, 1), 2, 1)
  cuts <- df5$maxx[h:(nrow(df5) - t)]
  return(list(df = manual_bin(data, yname, xname, cuts = cuts), 
              cuts = cuts))
}

###############################################################################

gbm_bin <- function(data, y, x) {
# INPUT   
# data: input dataframe
# y   : name of Y in the input dataframe with binary 0/1 values
# x   : name of X in the input dataframe with numeric values

  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  df2 <- data.frame(y = df1[[yname]], x = df1[[xname]], x2 = df1[[xname]])
  spcor <- cor(df2[, 2], df2[, 1], method = "spearman", use = "complete.obs")

  mdl <- gbm::gbm(y ~ x + x2, distribution = "bernoulli", data = df2, var.monotone = c(spcor / abs(spcor), spcor / abs(spcor)), 
                  bag.fraction = 1, n.minobsinnode = round(nrow(df2) / 100))
  df3 <- data.frame(y = df2$y, x = df2$x, yhat = gbm::predict.gbm(mdl, n.trees = mdl$n.trees, type = "response"))
  
  df4 <- Reduce(rbind, 
           lapply(split(df3, df3$yhat), 
             function(x) data.frame(maxx = max(x$x), 
                                    yavg = mean(x$y),
                                    yhat = round(mean(x$yhat), 8))))

  df5 <- df4[order(df4$maxx), ]
  h <- ifelse(df5[["yavg"]][1] %in% c(0, 1), 2, 1)
  t <- ifelse(df5[["yavg"]][nrow(df5)] %in% c(0, 1), 2, 1)
  cuts <- df5$maxx[h:max(h, (nrow(df5) - t))]

  return(list(df   = manual_bin(data, yname, xname, cuts = cuts), 
              cuts = cuts))  
}

###############################################################################

batch_bin <- function(data, method) {
# INPUT
# data  : input dataframe with the binary Y in the last column
# method: an integer representing the binning algorithm, currently:
#         1 for the iteration, 2 for the iteration for bads only
#         3 for the isotonic regression, 4 for the generalized boosting model

  if (sum(Reduce(c, Map(is.numeric, data))) < ncol(data)) {
    stop("All variables in the data frame need to be numeric !")
  }

  mlist <- c("qtl_bin", "bad_bin", "iso_bin", "gbm_bin")
  if (method %in% 1:length(mlist)) {
    source(paste(mlist[method], ".R", sep = ''))
    bin_fn <- get(mlist[method])
  } else {
    stop("The method is not supported !")
  }

  df1 <- subset(data, data[, ncol(data)] %in% c(0, 1))
  xnames <- colnames(df1)[1:(ncol(df1) - 1)]
  yname <- colnames(df1)[ncol(df1)]

  xlst <- parallel::mclapply(xnames, mc.cores = parallel::detectCores(), 
            function(xname) bin_fn(data.frame(y = df1[[yname]], x = df1[[xname]]), y, x))
  names(xlst) <- xnames

  xsum <- Reduce(rbind, parallel::mclapply(xnames, mc.cores = parallel::detectCores(),
            function(xname) data.frame(var    = xname,
                                       nbin   = nrow(xlst[[xname]]$df),
                                       unique = length(unique(df1[!is.na(df1[[xname]]), xname])),
                                       miss   = sum(xlst[[xname]]$df[["mv_cnt"]]),
                                       min    = min(df1[!is.na(df1[[xname]]), xname]),
                                       median = median(df1[!is.na(df1[[xname]]), xname]),
                                       max    = max(df1[!is.na(df1[[xname]]), xname]),
                                       ks     = max(xlst[[xname]]$df[["ks"]]),
                                       iv     = sum(xlst[[xname]]$df[["iv"]]))))

  BinOut <- list(BinSum = xsum, BinLst = xlst)
  class(BinOut) <- "BinSummary"
  return(BinOut)
}

print.BinSummary <- function(x) {
  print(x$BinSum)
}

###############################################################################

cal_woe <- function(data, xname, spec) {
# INPUT
# data : the input dataframe
# x    : the numeric variable to which the woe is applied
# spec : the output table from binning functions, e.g. mono_bin() or iso_bin()

  wname <- paste("woe", xname, sep = ".")

  calc <- function(i) {
    s <- spec[i, ]
    if (length(with(data, which(eval(parse(text = gsub("$X", xname, s$rule, fixed = T)))))) == 0) {
      return()
    }
    else {
      d <- data[with(data, which(eval(parse(text = gsub("$X", xname, s$rule, fixed = T))))), ]
      d$woe.bin <- s$bin
      return(within(d, assign(wname, s$woe)))
    }
  }
  df1 <- Reduce(rbind, Map(calc, seq(nrow(spec))))

  sm1 <- Reduce(rbind,
           Map(function(x) data.frame(bin      = unique(x$woe.bin), 
                                      cal_freq = nrow(x),
                                      cal_dist = round(nrow(x) / nrow(df1), 4), 
                                      cal_woe  = mean(x[[wname]])),
             split(df1, df1$woe.bin, drop = T)))
  
  ### MERGE THE SPEC TABLE AND THE WOE SUMMARY TABLE ###
  sm2 <- merge(spec[, c("bin", "rule", "dist", "woe")], sm1, by = c("bin"), all = T)
  sm2$psi <- round((sm2$cal_dist - sm2$dist) * log(sm2$cal_dist / sm2$dist), 4)
  woe_out <- list(df = df1, psi = sm2)
  class(woe_out) <- "psi"
  return(woe_out)
}

print.psi <- function(x) {
  print(x$psi)
}

###############################################################################

batch_woe <- function(data, slst) {
# INPUT
# data : input dataframe with all independent variables to which woe transformations would be applied
# slst : a list of woe specification that is the output from batch_bin(), e.g. batch_bin(...)$BinLst
#        alternatively, it can be a list of outputs from each binning function, e.g. gbm_bin(...)$df

  if (length(intersect(names(slst), colnames(data))) == 0) {
    stop('The data frame and the woe spec do not match !')
  } else {
    xnames <- intersect(names(slst), colnames(data))
    spec <- slst[xnames]
    df1 <- cbind(idx_ = seq(nrow(data)), data[, xnames])
  }

  woe <- parallel::mclapply(xnames, mc.cores = parallel::detectCores(),
           function(xname) cal_woe(df1, xname, spec[[xname]]$df))
  names(woe) <- xnames

  psi <- lapply(woe, function(x) x$psi)
  df2 <- Reduce(merge, parallel::mclapply(woe, mc.cores = parallel::detectCores(),
           function(x) x$df[order(x$df[["idx_"]]), c(1, ncol(x$df))]))
  out <- list(psi = psi, df = df2)
  class(out) <- "psiSummary"
  return(out)
}

print.psiSummary <- function(x) {
  psi <- Reduce(cbind, lapply(x$psi, function(i) data.frame(sum(i$psi))))
  dimnames(psi) <- list("psi", names(x$psi))
  print(psi)
}

###############################################################################

wts_bin <- function(spec, w) {
# spec : the output table from binning functions, e.g. mono_bin() or iso_bin()
# w    : a two-value vector with the first being the good weight and the second being the bad weight
 
  gw = w[1]
  bw = w[2]
  df1 <- within(spec, {
    wt_bads <- bad_freq * bw
    wt_good <- (freq - bad_freq) * gw
    wt_freq <- wt_bads + wt_good 
    wt_dist <- wt_freq / sum(wt_freq)
    wt_woe  <- log((wt_bads / sum(wt_bads)) / (wt_good / (sum(wt_good))))
    wt_iv   <- (wt_bads / sum(wt_bads) - wt_good / sum(wt_good)) * wt_woe
    wt_ks   <- abs(cumsum(wt_bads) / sum(wt_bads) - cumsum(wt_good) / sum(wt_good)) * 100
  })

  return(data.frame(bin        = df1$bin,
                    rule       = df1$rule,
                    wt_freq    = round(df1$wt_freq, 2),
                    wt_dist    = round(df1$wt_dist, 4),
                    wt_bads    = round(df1$wt_bads, 2),
                    wt_badrate = round(df1$wt_bads / df1$wt_freq, 4),
                    wt_woe     = round(df1$wt_woe, 4),
                    wt_iv      = round(df1$wt_iv, 4),
                    wt_ks      = round(df1$wt_ks, 4))) 
}

###############################################################################

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

###############################################################################

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
