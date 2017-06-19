#' @title
#' Calculate Direct Wind Component
#' @description
#' Function to calculate direct wind component from total wind vector.
#' @param True_Heading_deg True aircraft heading in degrees of bearing (North == 0)
#' @param True_Wind_Direction_From_Deg True wind direction (from) in degrees of bearing (North == 0)
#' @param Wind_Speed_kts Wind speed in knots
#' @return
#' Direct wind component in knots (headwind = positive; tailwind = negative)
#' @seealso
#' \code{\link{windVectors_crosswind}}
#' @examples
#' windVectors_headwind(0, 270, 10) # Direct crosswind has no direct wind component
#' windVectors_headwind(0, 0, 10) # Headwind has a positive value
#' windVectors_headwind(0, 150, 10) # Tailwind has a negative value
#' windVectors_headwind(0, 540, 10) # Degree values >=360 can be used (360 == 0)
windVectors_headwind <- function(True_Heading_deg, True_Wind_Direction_From_deg, Wind_Speed_kts) {
  # Calculate alpha (angle between aircraft and wind)
  alpha <- True_Wind_Direction_From_deg - True_Heading_deg
  # Calculate direct wind component
  Direct_Wind_Component <- cos(alpha*pi/180) * Wind_Speed_kts
  return(Direct_Wind_Component)
}
