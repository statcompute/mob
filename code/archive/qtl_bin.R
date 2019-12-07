qtl_bin <- function(data, y, x) {
# INPUT
# data: input dataframe
# y   : name of Y in the input dataframe with 0/1 binary values
# x   : name of X in the input dataframe with numeric values
  
  yname <- deparse(substitute(y))
  xname <- deparse(substitute(x))
  df1 <- subset(data, !is.na(data[[xname]]) & data[[yname]] %in% c(0, 1), select = c(xname, yname))
  pt0 <- unique(Map(function(n_) unique(quantile(df1[[xname]], prob = seq(0, 1, 1 / n_), names = F, type = 3)),
                    2:min(50, length(unique(df1[[xname]])))))
  pts <- pt0[which(Map(length, pt0) > 2)]
  cut <- Map(function(p_) cut(df1[[xname]], breaks = p_, include.lowest = T, labels = F), pts)
  mns <- Map(function(c_) Reduce(rbind, Map(function(d_) data.frame(xmn = mean(d_[[xname]]), ymn = mean(d_[[yname]])),
                                            split(df1, c_))), cut)
  chk <- Reduce(rbind, Map(function(s_) data.frame(scor = round(abs(cor(s_$ymn, s_$xmn, method = "spearman", use = "complete.obs")), 8),
                                                   rcnt = nrow(s_), miny = min(s_$ymn), maxy = max(s_$ymn)), mns))
  sel <- tail(subset(cbind(chk, pts = I(pts)), scor == 1 & miny > 0 & maxy < 1), 1)$pts[[1]]
  return(list(df   = manual_bin(data, yname, xname, cuts = sel[2:(length(sel) - 1)]),
              cuts = sel[2:(length(sel) - 1)]))
}
