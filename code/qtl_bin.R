#' Monotonic binning by quantile
#'
#' The function \code{qtl_bin} implements quantile-based monotonic binning.
#' 
#' @param data A input dataframe 
#' @param y    The name of Y with 0/1 binary values
#' @param x    The name of X with numeric values
#'
#' @return A list of binning outcomes, including a list of cut points and a summary dataframe
#'
#' @examples 
#' qtl_bin(df, bad, majordrg)

qtl_bin <- function(data, y, x) {
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))  
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  nbin <- 2

  if (length(unique(df1[[xname]])) == 1) {
    stop(paste("there is only a single value in", xname), call. = F)
  } else if (length(unique(df1[[xname]])) == 2) {
    return(list(df   = manual_bin(data, yname, xname, cuts = min(unique(df1[[xname]]))), 
                cuts = min(unique(df1[[xname]]))))
  } else {
    repeat {
      pts <- Hmisc::cut2(df1[[xname]], g = nbin + 1, onlycuts = T)
      df1$cut <- cut(df1[[xname]], breaks = pts, include.lowest = T)
      df2 <- Reduce(rbind, 
               Map(function(x) data.frame(xmean = mean(x[[xname]]), ymean = mean(x[[yname]])),
                 split(df1, df1$cut)))
      flg1 <- ifelse(round(abs(cor(df2$xmean, df2$ymean, method = "spearman", use = "complete.obs")), 8) < 1, 1, 0)
      flg2 <- ifelse(max(df2$ymean) == 1 | min(df2$ymean) == 0, 1, 0)
      if ((flg1 + flg2) > 0) {
        cuts <- Hmisc::cut2(df1[[xname]], g = nbin, onlycuts = T)
        break
      }
      nbin <- nbin + 1
    }
    return(list(df   = manual_bin(data, yname, xname, cuts = cuts[2:(length(cuts) - 1)]), 
                cuts = cuts[2:(length(cuts) - 1)]))
  }
}
