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
                     title = NA,
                     type = c("np", "p"),
                     group = c("monthly", "quarterly"),
                     enddate = floor_date(today(), "month") - 1, # Last day of previous month
                     startdate = enddate - months(24) + 1) { # Go 24 months back
  
  # Remove warnings
  options(warn = -1)
  
  # Check for filename
  if(is.null(filename)) {
    stop("Filename argument is required")
  }
  
  # Required packages without exposing messages
  suppressMessages(require(lubridate))
  suppressMessages(require(tidyverse))
  suppressMessages(require(qcc))
  
  # Read input files
  df <- suppressMessages(read_csv(filename))

  # Check for legitimate arguments
  if(!(is.data.frame(df)) | ncol(df) != 3) {
    stop("Data file not recognized as a data.frame with three columns")
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

  # Clean up input data file
  # [Re-]name columns based on position
  df <- df %>%
    select(Date_Label_Month = 1,
           Events = 2,
           Operations = 3)

  # Convert date labels into R Date format, necessary for grouping and filtering
  dates_list <- df %>%
    select(Date_Label_Month) %>%
    unlist() %>%
    strsplit(",")

  months_vector <- dates_list %>%
    sapply("[", 1) %>%
    unname()

  years_vector <- dates_list %>%
    sapply("[", 2) %>%
    unname() %>%
    as.integer()

  dates_vector <- paste(years_vector, months_vector, "15", sep = "-") %>%
    as.Date(format = "%Y-%b-%d") %>%
    ceiling_date("month") - 1

  df <- df %>%
    mutate(Date_Collected = dates_vector) %>%
    mutate(Quarter_int = quarter(Date_Collected)) %>%
    mutate(Year_int = year(Date_Collected)) %>%
    mutate(Date_Label_Quarter = paste(paste("Q", Quarter_int, sep = ""),
                                      Year_int, sep = ", ")) %>%
    select(-Quarter_int, -Year_int)

  # Apply date filter
  df <- df %>%
    filter(Date_Collected >= startdate, Date_Collected <= enddate)

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
      group_by(Date_Label_Quarter) %>%
      summarise(Events = sum(Events), Operations = sum(Operations)) %>%
      ungroup() %>%
      select(Date_Label = Date_Label_Quarter,
             Events = Events,
             Operations = Operations)

    # Must re-order data.frame after grouping to preserve time series order
    dates_list_ordering <- df_grouped %>%
      select(Date_Label) %>%
      unlist() %>%
      strsplit(", ")

    quarters_vector_ordering <- dates_list_ordering %>%
      sapply("[", 1) %>%
      unname()

    years_vector_ordering <- dates_list_ordering %>%
      sapply("[", 2) %>%
      unname() %>%
      as.integer()

    df_grouped <- df_grouped %>%
      mutate(Year_int = years_vector_ordering) %>%
      mutate(Quarter_int = quarters_vector_ordering) %>%
      arrange(Year_int, Quarter_int) %>%
      select(-Year_int, -Quarter_int)
  }

  # Create quality control chart
  # Collect variables for QCC calculation
  observations <- df_grouped %>%
    select(Events) %>%
    unlist() %>%
    unname()

  labels <- df_grouped %>%
    select(Date_Label) %>%
    unlist() %>%
    unname()

  sizes <- df_grouped %>%
    select(Operations) %>%
    unlist() %>%
    unname()

  # Chart type
  chartType <- type[1]
  centerLabel <- case_when(chartType == "np" ~ "Average Events Adjusted for Flights",
                           chartType == "p" ~ "Average Events")
  yAxisLabel <- case_when(chartType == "np" ~ "Count of Events",
                           chartType == "p" ~ "Proportion of Events")
  
  # Calculate QCC values
  qcc_calc <- qcc(data = observations, type = chartType, sizes = sizes, plot = FALSE)

  qcc_df <- data.frame(labels = labels,
                           center = qcc_calc$center,
                           values = qcc_calc$statistics,
                           LCL = qcc_calc$limits[,1],
                           UCL = qcc_calc$limits[,2],
                           beyond_limits = 0,
                           stringsAsFactors = FALSE)

  # Convert labels into factor so ggplot will know to order it non-alphabetically
  qcc_df$labels <- factor(qcc_df$labels,
                              levels = unique(qcc_df$labels))

  # Update data.frame with rows where runs rules were exceeded
  runs_index <- unname(unlist(qcc_calc$violations[2]))
  qcc_df[runs_index, "beyond_limits"] <- 2

  # Update data.frame with rows where limit was exceeded
  beyond_limits_index <- unname(unlist(qcc_calc$violations[1]))
  qcc_df[beyond_limits_index, "beyond_limits"] <- 1

  # scale_color_manual values
  if(chartType == "np") {
    scale_values <- c("Control Limits Adjusted for Flights" = "red", "Average Events Adjusted for Flights" = "blue", "Count of Events" = "black")
  }
  if(chartType == "p") {
    scale_values <- c("Control Limits Adjusted for Flights" = "red", "Average Events" = "blue", "Proportion of Events" = "black")
  }
  
  # Create ggplot
  plot <- ggplot(qcc_df) +
    labs(title = title, x = "Dates", y = yAxisLabel[1]) +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 14),
          title = element_text(size = 18),
          legend.text = element_text(size = 12),
          legend.position = "bottom") +
    geom_step(aes(x = labels, y = LCL, color = "Control Limits Adjusted for Flights"), linetype = 2, group = 1) +
    geom_step(aes(x = labels, y = UCL, color = "Control Limits Adjusted for Flights"), linetype = 2, group = 1) +
    geom_step(aes(x = labels, y = center, color = centerLabel[1]), linetype = 2, group = 1) +
    geom_line(aes(x = labels, y = values, color = yAxisLabel[1]), group = 1) +
    geom_point(aes(x = labels, y = values, fill = factor(beyond_limits)), shape = 21 , color = "transparent" , size = 4) +
    scale_fill_manual(values = c("0" = "black", "1" = "red", "2" = "yellow"), guide = FALSE) +
    scale_color_manual(name = "", values = scale_values)
  
  # Bring back warnings
  options(warn = 0)
  
  # Plot
  return(plot)
  }
