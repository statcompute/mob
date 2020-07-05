#' Monotonic binning based on isotonic regression
#'
#' The function \code{iso_bin} implements monotonic binning based on isotonic  regression
#'
#' @param data A input dataframe
#' @param y    The name of Y with 0/1 binary values
#' @param x    The name of X with numeric values
#'
#' @return A list of binning outcomes, including a list of cut points and a summary dataframe
#'
#' @examples
#' iso_bin(df, bad, majordrg)

iso_bin <- function(data, y, x) {
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))

  if (length(unique(df1[[xname]])) == 1) {
    stop(paste("there is only a single value in", xname), call. = F)
  } else if (length(unique(df1[[xname]])) == 2) {
    return(list(df   = manual_bin(data, yname, xname, cuts = min(unique(df1[[xname]]))),
                cuts = min(unique(df1[[xname]]))))
  } else {
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
}
