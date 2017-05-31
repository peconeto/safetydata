# Formulas to calculate intersect of theoretical quantiles
# Combine with qq_abline_slope()
# To be used with ggplot2::geom_qq()
# Inspired from stats::qqline()
# x is a numeric vector
qq_abline_intersect <- function(x, slope = NULL) {
  # If slope is not provided, use qq_abline_slope() to calculate it
  if (is.null(slope)) {
    slope <- qq_abline_slope(x)
  }
  
  x <- x[!is.na(x)]
  intersect <- quantile(x, 0.25) - slope * qnorm(0.25)
  return(intersect)
}