# The function EAS_to_TAS converts equivalent airspeed to true airspeed
# Note: For most modern commercial operators, EAS is approximately equal to CAS, which is approximately equal to IAS
# EAS_kts = Equivalent airspeed in knots
# Altimeter_inHg = Local altimeter setting in inHg
# Temp_C = Local temperature in C

# Source: http://www.faa.gov/regulations_policies/handbooks_manuals/aviation/media/00-80t-80.pdf
EAS_to_TAS <- function(EAS_kts, Altimeter_inHg = 29.92, Temp_C = 15) {
  # Constants
  p <- Altimeter_inHg * 33.8639 # Pressure converstion from altimeter (inHg) to hPa / millibars
  Temp <- Temp_C + 273.15 # Temperature conversion from C to K
  p0 <- 1013.2 # Standard sea level pressure in hPa / millibars
  T0 <- 288.15 # Standard sea level temperature in K
  d0 <- 1.225 # Standard sea level density in kg/m^3
  
  # Calculate local air density
  d <- d0 * ((p * T0) / (p0 * Temp))
  
  # Perform EAS->TAS calculation
  TAS <- EAS_kts * sqrt(d0 / d)
  
  # Return TAS
  return(TAS)
}
