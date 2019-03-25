calc_woe <- function(data, x, spec) {

  ### GET THINGS READY ###
  xname <- deparse(substitute(x)) 
  wname <- paste("woe", xname, sep = ".")
  
  ### DEFINE A FUNCTION TO CALCULATE WOE ###
  calc <- function(i) {
    s <- spec[i, ]
    d <- data[with(data, which(eval(parse(text = gsub("$X", xname, s$rule, fixed = T))))), ]
    d$wbin <- s$bin
    return(within(d, assign(wname, s$woe)))
  }
  df1 <- Reduce(rbind, Map(calc, seq(nrow(spec))))
 
  ### SUMMARIZE THE DATA BY EACH WOE CATEGORY ###
  sm1 <- Reduce(rbind,
           Map(function(x) data.frame(bin      = unique(x$wbin),
                                      cal_freq = nrow(x),
                                      cal_dist = round(nrow(x) / nrow(df1), 4),
                                      cal_woe  = mean(x[[wname]])),
             split(df1, df1$wbin)))
  
  ### MERGE THE SPEC TABLE AND THE WOE SUMMARY TABLE ###
  sm2 <- merge(spec[, c("bin", "rule", "dist", "woe")], sm1, by = c("bin"), full = T)
  sm2$psi <- round((sm2$cal_dist - sm2$dist) * log(sm2$cal_dist / sm2$dist), 4)
  df1$wbin <- NULL
  return(list(df = df1, psi = sm2));
} 
