#' @title
#' Rare Event Search
#' @description
#' A function used to determine rare events that require validation in EMS
#' @param username eFOQA username
#' @param password eFOQA password
#' @param metadata.db file name of metadata
#' @param metadata.exists logical. Does the metadata file exist in the working directory?
#' @param metadata.keep logical. Should the metadata file be kept after running the function? Note: Due to permissioning, the metadata file may be kept even if metadata.keep = FALSE
#' @param path specify metadata file location for saving and retrieval
#' @return
#' events that require validation in EMS that can be saved as a data frame
#' @examples
#' rare_events('username', 'password', \cr
#' metadata.db = 'event_validation.db', \cr
#' metadata.exists = FALSE, metadata.keep = FALSE, \cr
#' path = getwd()))
#' @export
rare_events <- function(username, password, metadata.db = 'event_validation.db', metadata.exists = FALSE, metadata.keep = FALSE,  path = getwd()) {

  # Load required libraries
  require(Rems)
  require(lubridate)
  require(safetydata)
  require(tidyverse)

  ems_name <- 'ems4-app'

  if(file.exists(metadata.db) & metadata.exists == FALSE) {invisible(readline(prompt="A metadata file already exists in your working directory. Press [enter] to overwrite the existing metadata file or [esc] to exit function."))}
  if(exists("fq", envir = .GlobalEnv)) {invisible(readline(prompt="fq already exists in your Global Environment. Press [enter] to overwrite the object or [esc] to exit function."))}
  if(exists("df1", envir = .GlobalEnv)) {invisible(readline(prompt="df1 already exists in your Global Environment. Press [enter] to overwrite the object or [esc] to exit function."))}
  if(exists("eventdfs", envir = .GlobalEnv)) {invisible(readline(prompt="eventdfs already exists in your Global Environment. Press [enter] to overwrite the object or [esc] to exit function."))}
  if(exists("eventtypeeua", envir = .GlobalEnv)) {invisible(readline(prompt="eventtypeeua already exists in your Global Environment. Press [enter] to overwrite the object or [esc] to exit function."))}
  if(exists("eventtypetoconfig", envir = .GlobalEnv)) {invisible(readline(prompt="eventtypetoconfig already exists in your Global Environment. Press [enter] to overwrite the object or [esc] to exit function."))}
  if(exists("rareevents", envir = .GlobalEnv)) {invisible(readline(prompt="rareevents already exists in your Global Environment. Press [enter] to overwrite the object or [esc] to exit function."))}
  if(length(ls(pattern="^dfrareevents", envir=.GlobalEnv) != 0)) {invisible(readline(prompt="dfrareevents already exists in your Global Environment. Press [enter] to overwrite the object or [esc] to exit function."))}

  # Query and save metadata

  # Global variables of event types (EMS API Work-Around)
  assign("eventtypetoconfig", c('E190 Not in T/O Config excluding autobrakes', 'Airbus Not in T/O Config excluding autobrakes'), envir = .GlobalEnv)
  assign("eventtypeeua", c('<-1700 below 500ft. HAT', 'Gear Down and Locked below 500ft. HAT',
                           'Late Final Flap Extension and Handle Movement Below 500ft. HAT',
                           'Speed <-15 Kts below 500ft. HAT vapp',
                           'Speed >40 Kts below 500ft. HAT vapp'), envir = .GlobalEnv)

  # Create tibble of metadata fields to be queried
  metadata_event_fields_df <-
    # P0: GPWS & Stall
    tibble(search_name = "gpws_stall",
           col_update_dbtree = list(c('apm events', 'standard lib', 'p0')),
           col_set_database = list(c('p0: library flight safety events - all events')),
           profile = "P0",
           eventtype = "c('GPWS: Terrain', 'GPWS: Pull Up', 'GPWS: Terrain Pull Up', 'Stall Warning')",
           col_update_fieldtree =
             list(c('Profile 0: Event Information'),
                  c('Profiles', 'Standard Library Profiles', 'P0: Library Flight Safety Events', 'Processing Status'),
                  c('Flight Review', 'Data Quality')
             )
    ) %>%
    # P196: HSRTO
    bind_rows(tibble(search_name = "hsrto",
                     col_update_dbtree = list(c('apm events', 'foqa', 'event validation', 'p196: HSRTO Events')),
                     col_set_database = list(c('p196: HSRTO Events - all events')),
                     profile = "P196",
                     eventtype = "c('HSRTO')",
                     col_update_fieldtree =
                       list(c('Profile 196: Event Information'),
                            c('Profiles', 'FOQA', 'Event Validation', 'P196: HSRTO', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    # P172: Not in TO Config
    bind_rows(tibble(search_name = "toconfig",
                     col_update_dbtree = list(c('apm events', 'foqa', 'event validation', 'p172: not in t/o config events')),
                     col_set_database = list(c('p172: not in t/o config events - all events')),
                     profile = "P172",
                     eventtype = "eventtypetoconfig",
                     col_update_fieldtree =
                       list(c('Profile 172: Event Information'),
                            c('Profiles', 'FOQA', 'Event Validation', 'P172: Not in T/O Config', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    # P186: High Bank Angle
    bind_rows(tibble(search_name = "bankangle",
                     col_update_dbtree = list(c('apm events', 'foqa', 'event validation', 'p186: high bank angle profile events')),
                     col_set_database = list(c('p186: high bank angle profile events - all events')),
                     profile = "P186",
                     eventtype = "c('High Bank Angle for this Height')",
                     col_update_fieldtree =
                       list(c('Profile 186: Event Information'),
                            c('Profiles', 'FOQA', 'Event Validation', 'P186: High Bank Angle Profile', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    # P174: Operating Altitude Exceedance
    bind_rows(tibble(search_name = "opalitude",
                     col_update_dbtree = list(c('apm events', 'foqa', 'event validation', 'p174: Maximum Operating Altitude Events')),
                     col_set_database = list(c('p174: maximum operating altitude events - all events')),
                     profile = "P174",
                     eventtype = "c('190 Altitude Exceedance', '320/321 Exceedance')",
                     col_update_fieldtree =
                       list(c('Profile 174: Event Information'),
                            c('Profiles', 'FOQA', 'Event Validation', 'P174: Maximum Operating Altitude', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    # P176: Inadvertent Spoiler Deployment
    bind_rows(tibble(search_name = "spoilerdeployment",
                     col_update_dbtree = list(c('apm events', 'foqa', 'event validation', 'p176: Inadvertant Spoiler Deployment Events')),
                     col_set_database = list(c('p176: Inadvertant Spoiler Deployment Events - all events')),
                     profile = "P176",
                     eventtype = "c('Inadvertant Spoiler Deployment (as event)')",
                     col_update_fieldtree =
                       list(c('Profile 176: Event Information'),
                            c('Profiles', 'FOQA', 'Event Validation', 'P176: Inadvertant Spoiler Deployment', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    # P197: Low Fuel Landing
    bind_rows(tibble(search_name = "lowfuel",
                     col_update_dbtree = list(c('apm events', 'foqa', 'event validation', 'p197: low fuel landings events')),
                     col_set_database = list(c('p197: low fuel landings events - all events')),
                     profile = "P197",
                     eventtype = "c('A320 Low Fuel Landing', 'A321 Low Fuel Landing', 'E190 Low Fuel Landing')",
                     col_update_fieldtree =
                       list(c('Profile 197: Event Information'),
                            c('Profiles', 'FOQA', 'event validation', 'P197: low fuel landings', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    # P211: Low Energy Airbus
    bind_rows(tibble(search_name = "lowenergy",
                     col_update_dbtree = list(c('apm events', 'foqa', 'event validation', 'p211: risk of stall (low energy) events')),
                     col_set_database = list(c('p211: risk of stall (low energy) events - all events')),
                     profile = "P211",
                     eventtype = "c('Low Energy (Airbus)')",
                     col_update_fieldtree =
                       list(c('Profile 211: Event Information'),
                            c('Profiles', 'FOQA', 'event validation', 'P211: risk of stall (low energy)', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    # P203: Flap Handle Movement
    bind_rows(tibble(search_name = "flaphandle",
                     col_update_dbtree = list(c('apm events', 'foqa', 'event validation', 'p203: flap movement on takeoff events')),
                     col_set_database = list(c('p203: flap movement on takeoff events - all events')),
                     profile = "P203",
                     eventtype = "c('Flap Handle Movement on Takeoff')",
                     col_update_fieldtree =
                       list(c('Profile 203: Event Information'),
                            c('Profiles', 'FOQA', 'event validation', 'P203: flap movement on takeoff', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    # P205: Ground Spoilers
    bind_rows(tibble(search_name = "groundspoilers",
                     col_update_dbtree = list(c('apm events', 'foqa', 'event validation', 'p205: ground spoilers not deployed events')),
                     col_set_database = list(c('p205: ground spoilers not deployed events - all events')),
                     profile = "P205",
                     eventtype = "c('Ground Spoilers Not Deployed Airbus', 'Ground Spoilers Not Deployed Embraer')",
                     col_update_fieldtree =
                       list(c('Profile 205: Event Information'),
                            c('Profiles', 'FOQA', 'event validation', 'P205: ground spoilers not deployed', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    # P198: Egregious Unstable Approach
    bind_rows(tibble(search_name = "eua",
                     col_update_dbtree = list(c('apm events', 'FOQA', 'Event Validation', 'P198: Unstable Approach Events')),
                     col_set_database = list(c('p198: unstable approach events - all events')),
                     profile = "P198",
                     eventtype = "eventtypeeua",
                     col_update_fieldtree =
                       list(c('Profile 198: Event Information'),
                            c('Profiles', 'FOQA', 'Event Validation', 'P198: unstable approach', 'Processing Status'),
                            c('Flight Review', 'Data Quality')
                       )
    )) %>%
    mutate(search_factor = factor(search_name, levels = unlist(distinct(., search_name)))) %>% # Create factor of searches to correct sorting of search_group
    mutate(search_group = group_indices(., search_factor)) %>% # Create search_group
    group_by(search_group) %>%
    mutate(search_group_row = row_number()) %>% # Create search_group_row for each search_group
    ungroup() %>%
    select(search_group,
           search_group_row,
           search_name,
           profile,
           eventtype,
           col_update_dbtree,
           col_set_database,
           col_update_fieldtree) # Re-order and drop unnecessary columns for organization

  # Determine number search groups and of field trees in each search group
  indices <- metadata_event_fields_df %>%
    group_by(search_group) %>%
    summarise(max_search_group_row = max(search_group_row))

  # Query and save metadata, if file doesn't exist
  if(!file.exists(metadata.db) | metadata.exists == FALSE) {
    map(1:max(pull(indices, search_group)), function(gn) {

      flt_query(conn = connect(username, password), ems_name = ems_name, data_file = metadata.db) %>%
        update_dbtree(unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), col_update_dbtree))) %>%
        set_database(unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), col_set_database))) %>%
        generate_preset_fieldtree() %>% # Flight Information, Aircraft Information, and Navigation Information
        assign("fq", ., envir = .GlobalEnv)

      map(1:unlist(pull(filter(indices, search_group == gn), max_search_group_row)), function(rn) {
        fq %>%
          update_fieldtree(unlist(pull(filter(metadata_event_fields_df, search_group == gn, search_group_row == rn), col_update_fieldtree))) %>%
          assign("fq", ., envir = .GlobalEnv)
      })

      fq %>%
        save_metadata()

      fq <- fq %>%
        reset()
    })
  }


  # Query and save data
  tryCatch({
    map(1:max(pull(indices, search_group)), function(gn) {

      fq1 <- flt_query(conn = connect(username, password), ems_name = ems_name, data_file = metadata.db) %>%
        set_database(unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), col_set_database))) %>%
        Rems::select('Event Record',
                     'Flight Record',
                     'Flight Date',
                     'tail number',
                     'fleet',
                     'takeoff airport code',
                     'landing airport code',
                     paste0(unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), profile)), ': Event Type'),
                     paste0(unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), profile)), ': False Positive'),
                     paste0(unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), profile)), ': Status')
        ) %>%
        Rems::filter('"flight date" >= as.Date(paste(c(year(Sys.Date()), month(Sys.Date()), 01), collapse="-")) %m-% months(6)') %>%
        Rems::filter('"Data Quality (master)" == "Acceptable"') %>%
        Rems::filter('"takeoff airport code" != "UNKNOWN"') %>%
        Rems::filter(paste0('"', unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), profile)), ': Status" != "FOQA: Complete"')) %>%
        Rems::filter(paste0('"', unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), profile)), ': False Positive" == "Not a False Positive"')) %>%
        Rems::filter(paste0('"', unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), profile)), ': Event Type" in c(',
                            pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), eventtype), ')')) %>%
        Rems::filter(paste0('"', unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), profile)), ': Processing State" in c("Succeeded", "Reprocessing", "Need to Reprocess")'))

      # The following are exception filters that are only applied in certain scenarios - see documentation
      if(!(unlist(pull(filter(metadata_event_fields_df, search_group == gn & search_group_row == 1), search_name)) %in% c('hsrto', 'toconfig'))) {
        fq1 <- fq1 %>%
          Rems::filter('"landing airport code" != "UNKNOWN"')
      }

      fq1 %>%
        run() %>%
        as_tibble() %>%
        assign("df1", ., envir = .GlobalEnv)

      if(nrow(df1) != 0) {
        df1 %>%
          select(EventRecord = 1,
                 FlightRecord = 2,
                 FlightDate = 3,
                 Tail = 4,
                 Fleet = 5,
                 TakeoffAirport = 6,
                 LandingAirport = 7,
                 EventType = 8,
                 FalsePositive = 9,
                 Status = 10) %>%
          #bind_rows() %>%
          assign(paste0("df", "rareevents", gn), ., envir = .GlobalEnv)
      }
    })

  }, error=function(e) print("Error when conducting query. Delete metadata file from working directory and re-run rare_events function as first step to troubleshooting"))

  rareevents <- mget(ls(pattern="^dfrareevents", envir = .GlobalEnv), inherits =TRUE) %>%
    bind_rows() #%>%

  #if none, turn df1 to rareevents df
  ls(pattern="^dfrareevents", envir = .GlobalEnv) %>%
    assign("eventdfs", ., envir = .GlobalEnv)
  if(length(eventdfs) == 0) {assign("rareevents", df1)}


  # Delete metadata file
  if(metadata.keep == FALSE) {file.remove(metadata.db)}

  # Clean Global environment
  rm(fq, df1, eventdfs, eventtypeeua, eventtypetoconfig, envir=.GlobalEnv)
  rm(list=ls(pattern="^dfrareevents", envir=.GlobalEnv), envir=.GlobalEnv)

  return(rareevents)

}


