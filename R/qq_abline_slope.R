#' @title
#' Slope of Theoretical Normal Distribution
#' @description
#' Function to calculate the slope of the theoretical normal distribution for a vector of numbers.
#' This can be used to visually compare actual vs. theoretical values to make an interpretation on whether a sample of numbers is
#' likely driven from a normally distributed population. Calculations are largely inspired from \code{\link{stats::qqline}} function.
#' This can be used with \code{\link{ggplot2::geom_qq}} to create a Q-Q plot.
#' @seealso
#' \code{\link{qq_abline_intersect}}
#'
#' \code{\link{ggplot2::geom_qq}}
#'
#' \code{\link{stats::qqline}}
#' @param x A numeric vector
#' @return
#' An integer corresponding to a slope
#' @examples
#' qq_abline_slope(1:10)
qq_abline_slope <- function(x) {
  x <- x[!is.na(x)]
  slope <- diff(quantile(x, c(0.25, 0.75))) / diff(qnorm(c(0.25, 0.75)))
  return(slope)
}
