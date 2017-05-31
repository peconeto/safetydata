# The function windVectors_headwind extracts the direct wind component for a given wind direction and strength
# True_heading_deg = True aircraft heading in degrees (0-359)
# True_Wind_Direction = True wind direction FROM in degrees (0-359)
# Wind_Speed_kts = Wind speed in knots
# Returns a vector of wind strengths for the direct wind component
## Note: Positive values = head wind
## Note: Negative values = tail wind
windVectors_headwind <- function(True_Heading_deg, True_Wind_Direction_From_deg, Wind_Speed_kts) {
  # Calculate alpha (angle between aircraft and wind)
  alpha <- True_Wind_Direction_From_deg - True_Heading_deg
  
  # Calculate direct wind component
  Direct_Wind_Component <- cos(alpha*pi/180) * Wind_Speed_kts
  
  # Return direct wind component
  return(Direct_Wind_Component)
}
