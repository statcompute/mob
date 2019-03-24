manual_bin <- function(df, yname, xname, cuts) {
# df   : input dataframe
# yname: a string of Y in the dataframe
# xname: a string of X in the dataframe
# cuts : a numeric array
# 
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/manual_bin.R")
# manual_bin(df, "bad", "bureau_score", c(602, 621, 636, 649, 661, 676, 693, 717))
#   bin                           rule freq dist mv_cnt bad_freq bad_rate     woe     iv      ks
#    00                      is.na($X)  315 0.05    315      105   0.3333  0.6628 0.0282  4.2544
#    01                      $X <= 602  268 0.05      0      132   0.4925  1.3261 0.1075 12.3608
#    02           $X > 602 & $X <= 621  311 0.05      0      126   0.4051  0.9719 0.0636 18.9097
#    03           $X > 621 & $X <= 636  302 0.05      0      116   0.3841  0.8838 0.0503 24.6009
#    04           $X > 636 & $X <= 649  392 0.07      0      133   0.3393  0.6895 0.0382 30.1406
#    05           $X > 649 & $X <= 661  387 0.07      0      119   0.3075  0.5441 0.0227 34.3158
#    06           $X > 661 & $X <= 676  529 0.09      0      114   0.2155  0.0639 0.0004 34.9056
#    07           $X > 676 & $X <= 693  606 0.10      0      115   0.1898 -0.0956 0.0009 33.9413
#    08           $X > 693 & $X <= 717  844 0.14      0      128   0.1517 -0.3657 0.0173 29.2160
#    09                       $X > 717 1883 0.32      0      108   0.0574 -1.4435 0.4217  0.0000

  ### GET THE DATA PREPARED ###
  cuts <- sort(c(-Inf, cuts, Inf))
  df1 <- df[which(df[[yname]] %in% c(0, 1)), c(yname, xname)]
  all_cnt <- nrow(df1)
  all_bcnt <- sum(df1[[yname]])

  ### IDENTIFY DIFFERENT CASES WITH MISSING VALUES ###
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

  ### SLICE DATAFRAME BY CUT POINTS ###
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
  ### FIRST CASE FOR MISSING VALUES WITH BOTH GOODS AND BADS ###
  if (miss_flg == 1) {
    bin_summ <- rbind(data.frame(bin = sprintf("%02d", 0), xmin = NA, xmax = NA, cnt = mis_cnt, bcnt = mis_bcnt, mis_cnt = mis_cnt),
                      bin_summ)
  }
  ### SECOND CASE FOR MISSING VALUES WITH ONLY GOODS OR BADS ###
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

  ### PARSE BINNING RULES ###
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
  
  ### OUTPUT DATAFRAME ###
  return(data.frame(bin      = format(bin_summ$bin, width = 5, justify = "right"),
                    rule     = format(bin_summ$rule, width = 30, justify = "right"),
                    freq     = bin_summ$cnt,
                    dist     = round(bin_summ$dist, 2),
                    mv_cnt   = bin_summ$mis_cnt,  
                    bad_freq = bin_summ$bcnt,
                    bad_rate = round(bin_summ$brate, 4),
                    woe      = round(bin_summ$woe, 4), 
                    iv       = round(bin_summ$iv, 4), 
                    ks       = round(bin_summ$ks, 4)))
}
