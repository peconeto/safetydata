#' @title
#' Convert True Airspeed to Ground Speed
#' @description
#' Function to convert aircraft's true airspeed (TAS) to groundspeed (GS).
#' GS is equal to TAS adjusted for direct wind.
#' @seealso
#' \code{\link{EAS_to_TAS}}
#'
#' \code{\link{windVectors_headwind}}
#' @param TAS_kts True airspeed in knots
#' @param Direct_Wind_Component_kts Direct wind component in knots (headwind = positive; tailwind = negative), defaults to 0
#' @return
#' Groundspeed in knots
#' @examples
#' TAS_to_GS(TAS_kts = 100) # TAS == GS when direct wind is 0kts
#' TAS_to_GS(TAS_kts = 100, 10) # Headwind reduces GS
#' TAS_to_GS(TAS_kts = 100, -10) # Tailwind increases GS
#' @export
TAS_to_GS <- function(TAS_kts, Direct_Wind_Component_kts = 0) {
  GS <- TAS_kts - Direct_Wind_Component_kts
  return(GS)
}
