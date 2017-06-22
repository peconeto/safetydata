#' @title
#' Calculate Density Altitude
#' @description
#' Function to calculate density altitude using meteorological data available for pilots, airlines, etc.
#' This function uses the National Weather Service estimation
#' @param Altimeter_inHg Local altimeter setting measured as inches of mercury, defaults to 29.92 (ICAO standard atmosphere)
#' @param Temp_C Local outside air temperature in degrees Celcius, defaults to 15 (ICAO standard temperature)
#' @param DewPoint_C Local dew point in degrees Celcius, defaults to 0
#' @return
#' Density altitude in feet
#' @source
#' National Weather Service / National Oceanic and Atmospheric Administration
#' Density Altitude
#' \url{https://www.weather.gov/media/epz/wxcalc/densityAltitude.pdf}
#' Temperature Conversion
#' \url{http://www.weather.gov/media/epz/wxcalc/tempConvert.pdf}
#' Pressure Conversion
#' \url{http://www.weather.gov/media/epz/wxcalc/pressureConversion.pdf}
#' @examples
#' densityAltitude() # Default density altitude is nearly 0 ft
#' densityAltitude(Altimeter_inHg = 29.50) # Density altitude increases as pressure decreases
#' densityAltitude(Temp_C = 30) # Density altitude increases with temperature
#' densityAltitude(DewPoint_C = 15) # Density altitude increases with the dew point when temperature is held constant since moist air is less dense than dry air
#' @export
densityAltitude <- function(Altimeter_inHg = 29.92, Temp_C = 15, DewPoint_C = 0) {
  # Conversions
  p <- Altimeter_inHg * 33.8639 # Pressure converstion from altimeter (inHg) to hPa / millibars
  Temp_K <- Temp_C + 273.15 # Convert temperature from Celcius to Kelvin
  # Calculate density altitude
  e = 6.11 * 10 * ((7.5 * DewPoint_C) / (237.7 + DewPoint_C))
  Tv <- Temp_K / (1 - ((e / p) * (1 - 0.622))) # Calculate virtual temperature in Kelvin
  Tv_R <- ((9/5) * (Tv - 273.15) + 32) + 459.69 # Convert virtual temperature to Rankline
  DA <- 145366 * (1 - ((17.326 * Altimeter_inHg) / Tv_R) ^ .235) # Calculate density altitude
  return(DA)
}
