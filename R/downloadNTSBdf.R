#' @title
#' Download NTSB Accidents and Incidents Database
#' @description
#' Function to download a fresh copy of the NTSB accidents and incidents database from the web as a data.frame.
#' Note that a 2017-03-28 version of the NTSB database created with this function is available with the package, see NTSBData_20170328.
#' @return
#' A data.frame with one row per accident or incident and one column for each field the NTSB makes available.
#' @source
#' \url{https://app.ntsb.gov/aviationquery/}
#' @examples
#' df <- downloadNTSBdf() # Downloads and stores as .GlobalEnv object
#' @export
downloadNTSBdf <- function() {
  # Import and read latest NTSB database
  df <- read.csv("http://app.ntsb.gov/aviationquery/Download.ashx?type=csv",
                 sep="|",
                 na.strings="  ",
                 stringsAsFactors = FALSE)
  # Remove blank "X" column
  df <- as.data.frame(df[,-grep("X", colnames(df))])
  # Trim leading + trailing spaces from values
  trim <- function(x) {gsub("^\\s+|\\s+$", "", x)}
  df <- as.data.frame(sapply(df, trim), stringsAsFactors = FALSE)
  # Correct data types
  df$Investigation.Type <- factor(df$Investigation.Type)
  df$Event.Date <- as.Date(df$Event.Date, format = "%m/%d/%Y")
  df$Country <- factor(df$Country)
  df$Airport.Code <- factor(df$Airport.Code)
  df$Aircraft.Damage <- factor(df$Aircraft.Damage)
  df$Aircraft.Category <- factor(df$Aircraft.Category)
  df$Amateur.Built <- factor(df$Amateur.Built)
  df$Number.of.Engines <- factor(df$Number.of.Engines)
  df$Engine.Type <- factor(df$Engine.Type)
  df$Schedule <- factor(df$Schedule)
  df$Purpose.of.Flight <- factor(df$Purpose.of.Flight)
  df$Total.Fatal.Injuries <- as.integer(df$Total.Fatal.Injuries)
  df$Total.Serious.Injuries <- as.integer(df$Total.Serious.Injuries)
  df$Total.Minor.Injuries <- as.integer(df$Total.Minor.Injuries)
  df$Total.Uninjured <- as.numeric(df$Total.Uninjured)
  df$Weather.Condition <- factor(df$Weather.Condition)
  df$Broad.Phase.of.Flight <- factor(df$Broad.Phase.of.Flight)
  df$Report.Status <- factor(df$Report.Status)
  df$Publication.Date <- as.Date(df$Publication.Date, format = "%m/%d/%Y")
  return(df)
}
