# The function TAS_to_GS converts true airspeed to ground speed
# TAS_kts = True airspeed in knots
# Direct_Wind_kts = Local direct wind component in knots (note: negative = tailwind)

# Source: http://www.faa.gov/regulations_policies/handbooks_manuals/aviation/media/00-80t-80.pdf
TAS_to_GS <- function(TAS_kts, Direct_Wind_Component_kts) {
  # Convert TAS to GS
  GS <- TAS_kts - Direct_Wind_Component_kts
  
  # Return GS
  return(GS)
}
