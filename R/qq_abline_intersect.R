#' @title
#' y-Intercept of Theoretical Normal Distribution
#' @description
#' Function to calculate the y-intercept of the theoretical normal distribution for a vector of numbers.
#' This can be used to visually compare actual vs. theoretical values to make an interpretation on whether a sample of numbers is
#' likely driven from a normally distributed population. Calculations are largely inspired from \code{\link{stats::qqline}} function.
#' This can be used with \code{\link{ggplot2::geom_qq}} to create a Q-Q plot.
#'
#' Note: Requires a slope to be provided, otherwise uses \code{\link{qq_abline_slope}} to calculate it (but will not return slope).
#' @seealso
#' \code{\link{qq_abline_slope}}
#'
#' \code{\link{ggplot2::geom_qq}}
#'
#' \code{\link{stats::qqline}}
#' @param x A numeric vector
#' @return
#' An integer corresponding to a y-intercept
#' @examples
#' qq_abline_intersect(1:10)
#' @export
qq_abline_intersect <- function(x, slope = NULL) {
  # If slope is not provided, use qq_abline_slope() to calculate it
  if (is.null(slope)) {
    slope <- qq_abline_slope(x)
  }
  x <- x[!is.na(x)]
  intersect <- quantile(x, 0.25) - slope * qnorm(0.25)
  return(intersect)
}
