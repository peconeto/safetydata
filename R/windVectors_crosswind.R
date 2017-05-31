# The function windVectors_crosswind extracts the cross wind component for a given wind direction and strength
# True_heading_deg = True aircraft heading in degrees (0-359)
# True_Wind_Direction = True wind direction FROM in degrees (0-359)
# Wind_Speed_kts = Wind speed in knots
# Returns a vector of wind strengths for the cross wind component
## Note: Positive values = right cross wind
## Note: Negative values = left cross wind
windVectors_crosswind <- function(True_Heading_deg, True_Wind_Direction_From_deg, Wind_Speed_kts) {
  # Calculate alpha (angle between aircraft and wind)
  alpha <- True_Wind_Direction_From_deg - True_Heading_deg
  
  # Calculate cross wind component
  Cross_Wind_Component <- sin(alpha*pi/180) * Wind_Speed_kts
  
  # Return cross wind component
  return(Cross_Wind_Component)
}
