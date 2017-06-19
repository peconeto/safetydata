#' @title
#' NTSB Accidents and Incidents from 2017-03-28
#' @description
#' A copy of the NTSB accidents and incidents database from 2017-03-28.
#' Created using \code{\link{downloadNTSBdf}}
#' @format A data frame with 79557 rows and 31 variables:
#' \describe{
#'   \item{Event.Id}{Unique NTSB event ID}
#'   \item{Investigation.Type}{Factor indicating accident or incident}
#'   ...
#' }
#' @source \url{https://app.ntsb.gov/aviationquery/}
"NTSBData_20170328"

#' @title
#' IATA and ICAO Airport Codes from 2017-04-04
#' @description
#' A list of global ICAO and IATA airport code from 2017-04-04.
#' Useful for converting between the two since airport-related data is often available in one or the other format.
#' @format A data frame with 30908 rows and 2 variables:
#' \describe{
#'   \item{IATA_Code}{3-letter IATA codes}
#'   \item{ICAO_Code}{4-letter ICAO codes}
#' }
"AirportCodes_20170404"

#' @title
#' Airport Timezones from 2017-04-04
#' @description
#' A list of airport timezones from 2017-04-04.
#' @format A data frame with 3527 rows and 2 variables:
#' \describe{
#'   \item{Code}{4-letter ICAO codes}
#'   \item{Zulu_Offset}{Integer representing hours offset from Zulu (UTC)}
#' }
"AirportTimezones_20170404"

#' @title
#' 2010's USA Daylight Savings Dates from 2017-04-04
#' @description
#' A list of 2010-2019 USA daylight savings change dates from 2017-04-04.
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{Change_Name}{Name of change; either ST -> DST ("spring forward") or DST -> ST ("fall back")}
#'   \item{Change_Value}{Integer representing changed time in hours (+1 or -1)}
#'   \item{Date_Time}{Local daylight savings time change date and time}
#' }
#' @source \url{https://www.timeanddate.com/time/zone/usa/new-york}
"USA_DaylightSavings_20170404"
