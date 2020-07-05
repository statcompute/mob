#' Monotonic binning based on bad only
#'
#' The function \code{bad_bin} implements monotonic binning only based on bads, e.g. Y = 1 
#' 
#' @param data A input dataframe 
#' @param y    The name of Y with 0/1 binary values
#' @param x    The name of X with numeric values
#'
#' @return A list of binning outcomes, including a list of cut points and a summary dataframe
#'
#' @examples 
#' bad_bin(df, bad, majordrg)

bad_bin <- function(data, y, x) {
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  df2 <- subset(df1, df1[[yname]] == 1)
  nbin <- 2

  if (length(unique(df2[[xname]])) == 1) {
    stop(paste("there is only a single value in", xname, "when y = 1"), call. = F)
  } else if (length(unique(df2[[xname]])) == 2) {
    return(list(df   = manual_bin(data, yname, xname, cuts = min(unique(df2[[xname]]))), 
                cuts = min(unique(df2[[xname]]))))
  } else {
    repeat {
      pts <- Hmisc::cut2(df2[[xname]], g = nbin + 1, onlycuts = T)
      df1$cut <- cut(df1[[xname]], breaks = pts, include.lowest = T)
      df3 <- Reduce(rbind, 
               Map(function(x) data.frame(xmean = mean(x[[xname]]), ymean = mean(x[[yname]])),
                 split(df1, df1$cut)))
      flg1 <- ifelse(round(abs(cor(df3$xmean, df3$ymean, method = "spearman", use = "complete.obs")), 8) < 1, 1, 0)
      flg2 <- ifelse(max(df3$ymean) == 1 | min(df3$ymean) == 0, 1, 0)
      if ((flg1 + flg2) > 0) {
        cuts <- Hmisc::cut2(df2[[xname]], g = nbin, onlycuts = T)
        break
      }
      nbin <- nbin + 1
    }
    return(list(df   = manual_bin(data, yname, xname, cuts = cuts[2:(length(cuts) - 1)]),
                cuts = cuts[2:(length(cuts) - 1)]))
  }
}
