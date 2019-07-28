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
  cl <- parallel::makeCluster(parallel::detectCores(), type = "PSOCK")
  parallel::clusterExport(cl, c("df1", "spec", "cal_woe"), envir = environment())
  woe <- parallel::parLapply(cl, xnames,
                             function(xname) cal_woe(df1, xname, spec[[xname]]$df))
  names(woe) <- xnames

  psi <- lapply(woe, function(x) x$psi)
  df2 <- Reduce(merge,
                parallel::parLapply(cl, woe,
                                    function(x) x$df[order(x$df[["idx_"]]), c(1, ncol(x$df))]))
  parallel::stopCluster(cl)
  out <- list(psi = psi, df = df2)
  class(out) <- "psiSummary"
  return(out)
}

print.psiSummary <- function(x) {
  psi <- Reduce(cbind, lapply(x$psi, function(i) data.frame(sum(i$psi))))
  dimnames(psi) <- list("psi", names(x$psi))
  print(psi)
}
