#' @title
#' Statistical Process Control np- or p-Chart Using ggplot2
#' @description
#' Function to create statistical process control (SPC) np-chart or p-chart from aviation safety datasets.
#' Uses qcc:qcc() to generate SPC chart values and ggplot2 to generate the plot.
#' @param filename Name of CSV file on working directory to be consumed; expects specific formatting:
#' First column contains dates in the format "Mmm, YYYY"
#' Second column contains a count of events/issues/deficiencies
#' Third column contains the number of relevant operations (i.e. flights)
#' There should be no skipped months within dataset
#' Ordered from oldest date first (top) to newest date last (bottom)
#' @param title Optional plot title
#' @param type Chart type; accepts np or p, default np
#' @param group Date grouping for data to be plotted; accepts monthly or quarterly, default monthly
#' @param enddate Ending date ; accepts lubridate functions, default last day of previous month from current date
#' @param startdate Starting date for data to be plotted; accepts lubridate functions, default first day 24 months before current date
#' @return
#' SPC np-chart
#' @examples
#' spcChart("data.csv")
#' ggplot2::ggsave(filename = "np-Chart.png", plot = spcChart(filename = "file.csv"), device = "png", path = getwd(), height = 7, width = 14, units = "in")
#' @export
spcChart <- function(filename,
                     title = basename(filename),
                     type = c("np", "p"),
                     group = c("monthly", "quarterly"),
                     enddate = floor_date(today(), "month") - 1, # Last day of previous month
                     startdate = enddate - months(24) + 1, # Go 24 months back
                     upper = TRUE,
                     lower = TRUE) {

  # Remove warnings
  options(warn = -1)

  # Required packages without exposing messages
  suppressMessages(require(lubridate))
  suppressMessages(require(tidyverse))
  suppressMessages(require(qcc))

  # Read input files
  df <- suppressMessages(read_csv(filename))

  # Check for legitimate arguments
  if(!(is.data.frame(df)) | ncol(df) != 3) {
    stop("Data file not recognized as a data.frame with three columns, or failed parsing")
  }
  if(!(group[1] %in% c("monthly", "quarterly"))) {
    stop("Grouping type not recognized")
  }
  if(!is.Date(startdate) | !(is.Date(enddate))) {
    stop("Start and end dates not recognized")
  }
  if(!type[1] %in% c("np", "p")) {
    stop("Only np or p chart types accepted")
  }
  if(nrow(df) == 0) {
    stop("Data.frame provided has no rows")
  }

  # Clean up input data file
  # Name columns based on position
  df <- df %>%
    select(Date_Label_Month = 1,
           Events = 2,
           Operations = 3)

  # Convert date labels into R Date format, necessary for grouping and filtering
  df <- df %>%
    separate(Date_Label_Month, c("Month", "Year", sep = ", "), remove = FALSE) %>%
    select(-`, `) %>%
    unite(Date_Collected, c(Year, Month), sep = "-") %>%
    mutate(Date_Collected = paste(Date_Collected, "15", sep = "-")) %>%
    mutate(Date_Collected = ymd(Date_Collected)) %>%
    mutate(Date_Collected = ceiling_date(Date_Collected, "month") - 1) %>%
    mutate(Quarter_int = quarter(Date_Collected)) %>%
    mutate(Year_int = year(Date_Collected)) %>%
    mutate(Date_Label_Quarter = paste(paste("Q", Quarter_int, sep = ""),
                                      Year_int, sep = ", "))# %>% select(-Quarter_int, -Year_int)

  # Apply date filter
  df <- df %>%
    filter(Date_Collected >= startdate, Date_Collected <= enddate)

  # Check for any remaining values
  if(nrow(df) == 0) {
    stop("Date filter has removed all rows from data.frame")
  }

  # Apply grouping
  if(group[1] == "monthly") {
    # Note that data is already grouped by month from input
    # No additional grouping operation is required here
    df_grouped <- df %>%
      select(Date_Label = Date_Label_Month,
             Events = Events,
             Operations = Operations)
  }
  if(group[1] == "quarterly") {
    df_grouped <- df %>%
      group_by(Date_Label_Quarter, Year_int, Quarter_int) %>%
      summarise(Events = sum(Events), Operations = sum(Operations)) %>%
      ungroup() %>%
      arrange(Year_int, Quarter_int) %>%
      select(Date_Label = Date_Label_Quarter,
             Events = Events,
             Operations = Operations)
  }

  # Create chart labels based on type
  chartType <- type[1]
  centerLabel <- case_when(chartType == "np" ~ "Average Events Adjusted for Flights",
                           chartType == "p" ~ "Average Events")
  yAxisLabel <- case_when(chartType == "np" ~ "Count of Events",
                           chartType == "p" ~ "Proportion of Events")

  # Calculate QCC values
  qcc_calc <- qcc(data = pull(df_grouped, Events),
                  type = chartType,
                  sizes = pull(df_grouped, Operations),
                  plot = FALSE)

  qcc_df <- tibble(labels = pull(df_grouped, Date_Label),
                   center = qcc_calc$center,
                   values = qcc_calc$statistics,
                   LCL = qcc_calc$limits[,1],
                   UCL = qcc_calc$limits[,2],
                   beyond_limits = 0)

  # Convert labels into factor so ggplot will know to order it non-alphabetically
  qcc_df <- qcc_df %>%
    mutate(labels = factor(labels, levels = unique(pull(qcc_df, labels))))

  # Disable UCL or LCL depending on function input
  if(upper == FALSE) {
    qcc_df <- qcc_df %>%
      mutate(UCL = NA)
  }
  if(lower == FALSE) {
    qcc_df <- qcc_df %>%
      mutate(LCL = NA)
  }

  # Update data.frame with rows where limit was exceeded
  if(upper == TRUE) {
    qcc_df <- qcc_df %>%
      mutate(beyond_limits = if_else(values > UCL, 1, 0))
  }
  if(lower == TRUE) {
    qcc_df <- qcc_df %>%
      mutate(beyond_limits = if_else(values < LCL, 1, 0))
  }

  # scale_color_manual values
  if(chartType == "np") {
    scale_values <- c("Control Limits Adjusted for Flights" = "red", "Average Events Adjusted for Flights" = "blue", "Count of Events" = "black")
  }
  if(chartType == "p") {
    scale_values <- c("Control Limits Adjusted for Flights" = "red", "Average Events" = "blue", "Proportion of Events" = "black")
  }

  # Create ggplot
  plot <- ggplot(data = qcc_df) +
    labs(title = title, x = "Dates", y = yAxisLabel[1]) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 14),
          title = element_text(size = 18),
          legend.text = element_text(size = 12),
          legend.position = "bottom")

  # Lower boundary step later
  if(lower == TRUE) {
      plot <- plot +
        geom_step(aes(x = labels, y = LCL, color = "Control Limits Adjusted for Flights"), linetype = 2, group = 1)
      }

  # Upper boundary step later
  if(upper == TRUE) {
      plot <- plot +
        geom_step(aes(x = labels, y = UCL, color = "Control Limits Adjusted for Flights"), linetype = 2, group = 1)
      }

  # Finish creating ggplot
  plot <- plot +
    geom_step(aes(x = labels, y = center, color = centerLabel[1]), linetype = 2, group = 1) +
    geom_line(aes(x = labels, y = values, color = yAxisLabel[1]), group = 1) +
    geom_point(aes(x = labels, y = values, fill = factor(beyond_limits)), shape = 21 , color = "transparent" , size = 4) +
    scale_fill_manual(values = c("0" = "black", "1" = "red"), guide = FALSE) +
    scale_color_manual(name = "", values = scale_values)

  # Bring back warnings
  options(warn = 0)

  # Plot
  return(plot)
  }
