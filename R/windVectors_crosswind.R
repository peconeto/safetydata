#' @title
#' Calculate Crosswind Component
#' @description
#' Function to calculate crosswind component from total wind vector.
#' @param True_Heading_deg True aircraft heading in degrees of bearing (North == 0)
#' @param True_Wind_Direction_From_Deg True wind direction (from) in degrees of bearing (North == 0)
#' @param Wind_Speed_kts Wind speed in knots
#' @return
#' Crosswind component in knots (from right = positive; from left = negative)
#' @seealso
#' \code{\link{windVectors_crosswind}}
#' @examples
#' windVectors_crosswind(0, 0, 10) # Direct headwind has no crosswind component
#' windVectors_crosswind(0, 90, 10) # Crosswind coming from the right has a positive value
#' windVectors_crosswind(0, 350, 10) # Crosswind coming from the left has a negative value
#' windVectors_crosswind(0, 630, 10) # Degree values >=360 can be used (360 == 0)
windVectors_crosswind <- function(True_Heading_deg, True_Wind_Direction_From_deg, Wind_Speed_kts) {
  # Calculate alpha (angle between aircraft and wind)
  alpha <- True_Wind_Direction_From_deg - True_Heading_deg
  # Calculate cross wind component
  Cross_Wind_Component <- sin(alpha*pi/180) * Wind_Speed_kts
  return(Cross_Wind_Component)
}
