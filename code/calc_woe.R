calc_woe <- function(data, x, spec) {
# data : the input dataframe
# x    : the numeric variable to which the woe is applied
# spec : the output table from binning functions, e.g. mono_bin() or iso_bin()
# 
# source("https://raw.githubusercontent.com/statcompute/MonotonicBinning/master/code/calc_woe.R")

  ### GET THINGS READY ###
  xname <- deparse(substitute(x))
  wname <- paste("woe", xname, sep = ".")

  ### DEFINE A FUNCTION TO CALCULATE WOE ###
  calc <- function(i) {
    s <- spec[i, ]
    if (length(with(data, which(eval(parse(text = gsub("$X", xname, s$rule, fixed = T)))))) == 0) {
      return()
    }
    else {
      d <- data[with(data, which(eval(parse(text = gsub("$X", xname, s$rule, fixed = T))))), ]
      d$wbin <- s$bin
      return(within(d, assign(wname, s$woe)))
    }
  }
  df1 <- Reduce(rbind, Map(calc, seq(nrow(spec))))
  
  ### SUMMARIZE THE DATA BY EACH WOE CATEGORY ###
  sm1 <- Reduce(rbind,
           Map(function(x) data.frame(bin      = unique(x$wbin),
                                      cal_freq = nrow(x),
                                      cal_dist = round(nrow(x) / nrow(df1), 4),
                                      cal_woe  = mean(x[[wname]])),
             split(df1, df1$wbin, drop = T)))  
             
  ### MERGE THE SPEC TABLE AND THE WOE SUMMARY TABLE ###
  sm2 <- merge(spec[, c("bin", "rule", "dist", "woe")], sm1, by = c("bin"), all = T)
  sm2$psi <- round((sm2$cal_dist - sm2$dist) * log(sm2$cal_dist / sm2$dist), 4)
  df1$wbin <- NULL
  return(list(df = df1, psi = sm2))
} 
