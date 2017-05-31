# Formulas to calculate slope of theoretical quantiles
# Combine with qq_abline_intersect()
# To be used with ggplot2::geom_qq()
# Inspired from stats::qqline()
# x is a numeric vector
qq_abline_slope <- function(x) {
  x <- x[!is.na(x)]
  slope <- diff(quantile(x, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
  return(slope)
}