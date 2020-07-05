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
