# Function accepts a data file, start and end dates, and grouping
# Returns SPC chart
# Function expects certain standards on data file:
## First column contains dates in the format "Mmm, YYYY"
## Second column contains a count of events/issues/deficiencies
## Third column contains the number of relevant operations
## There should be no skipped months within dataset
## Oldest date first (top), newest date last (bottom)
# Data is collected at month end; mid-month date ranges will exclude the last month

spcChart <- function(filename = NA,
                     title = NA,
                     group = c("monthly", "quarterly"),
                     enddate = floor_date(today(), "month") - 1, # Last day of previous month
                     startdate = enddate - months(24) + 1) { # Go 24 months back

  # Required packages without exposing messages
  suppressMessages(require(lubridate))
  suppressMessages(require(tidyverse))
  suppressMessages(require(qcc))

  # Read input files
  df <- read.csv(filename, stringsAsFactors = FALSE)

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

  # Clean up input data file
  # Fix column names based on position
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

  # Apply filter
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

  # Calculate QCC values
  qcc_calc <- qcc(data = observations, type = "np", sizes = sizes, plot = FALSE)

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

  # Create ggplot
  plot <- ggplot(qcc_df) +
    labs(title = title, x = "Dates", y = "Count") +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 14),
          title = element_text(size = 18)) +
    geom_step(aes(x = labels, y = LCL), group = 1, linetype = 2, color = "red") +
    geom_step(aes(x = labels, y = UCL), group = 1, linetype = 2, color = "red") +
    geom_step(aes(x = labels, y = center), group = 1, linetype = 2, color = "blue") +
    geom_line(aes(x = labels, y = values), group = 1) +
    geom_point(aes(x = labels, y = values, color = factor(beyond_limits)), size = 4) +
    scale_colour_manual(values = c("0" = "black", "1" = "red", "2" = "yellow"), guide = FALSE)

  # Plot
  return(plot)
  }
