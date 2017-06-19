#' @title
#' Convert Equivalent Airspeed to True Airspeed
#' @description
#' Function to convert aircraft's equivalent airspeed (EAS) to true airspeed (TAS).
#' TAS is equal to EAS adjusted for non-standard atmospheric pressure and temperature.
#' For modern commercial aircraft that opearte below the speed of sound, indicated airspeed (IAS) and EAS are practically equal to each other.
#' They may be used interchangeably in the context of measuring aircraft speeds within these assuptions.
#' @seealso
#' \code{\link{TAS_to_GS}}
#' @param EAS_kts Equivalent airspeed in knots
#' @param Altimeter_inHg Local altimeter setting measured as inches of mercury, defaults to 29.92 (ICAO standard atmosphere)
#' @param Temp_C Local outside air temperature in degrees Celcius, defaults to 15 (ICAO standard temperature)
#' @return
#' True airspeed in knots
#' @source
#' US Navy, FAA (1965-01-02)
#' Aerodynamics for Naval Aviators (NAVAIR/NAVWEPS 00-80T-80)
#' \url{https://www.faa.gov/regulations_policies/handbooks_manuals/aviation/}
#' @source
#' NACA, ICAO, NASA (Published 1954-05-01, Acquired 1996-09-01)
#' Manual of the ICAO standard atmosphere calculations by the NACA (NASA Technical Reports Server Document ID 19930083952)
#' \url{https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19930083952.pdf}
#' @examples
#' EAS_to_TAS(EAS_kts = 100, Altimeter_inHg = 29.80, Temp_C = 35)
#' EAS_to_TAS(EAS_kts = 100) # At standard temperature and pressure, EAS is almost exactly the same as TAS
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
  return(TAS)
}
