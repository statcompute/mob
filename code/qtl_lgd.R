qtl_lgd <- function(data, y, x) {
# INPUT
# data: input dataframe
# y   : name of Y in the input dataframe with numeric values in the interval of [0, 1]
# x   : name of X in the input dataframe with numeric values

  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df0 <- subset(data, data[[yname]] <= 1 & data[[yname]] >= 0, select = c(xname, yname))
  df1 <- rbind(data.frame(x = df0[[xname]], y = 1, w = df0[[yname]]),
               data.frame(x = df0[[xname]], y = 0, w = 1 - df0[[yname]]))
  bin <- wqtl_bin(df1, y, x, w)
  bin$df[["cnt"]] <- NULL
  bin$df[["bad_freq"]] <- NULL
  names(bin$df)[names(bin$df) == "mv_wt"] <- "mv_cnt"
  names(bin$df)[names(bin$df) == "bad_rate"] <- "mean_y"
  return(bin)
}
