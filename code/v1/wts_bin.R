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
