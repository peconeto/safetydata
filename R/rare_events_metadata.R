#' @title
#' Metadata for Rare Events Search
#' @description
#' Function to create db file referenced in rare_events function
#' @param x eFOQA username in single quotes
#' @param y eFOQA password in single quotes
#' @return
#' Db file (rare_events2.db) saved in the user's working directory
#' @export
rare_events_metadata <- function(x, y) {

## Install and load Rems, if not already installed
if (exists('http_proxy')) {
library(httr)
set_config(use_proxy(url=http_proxy, port= proxy_port))
}

if (!require(devtools)) {
cat("\n==== Package devtools is not found. Installing devtools\n")
install.packages('devtools')
}

if(!require(Rems)) {
cat("\n==== Installing Rems")
devtools::install_github("ge-flight-analytics/Rems", dependencies = T)
}

## Load relevant packages
library(Rems)
library(magrittr)

## Connect to EFOQA and assign file name to metadata
conn <- connect(x, y)
metadata <- 'rare_events2.db'


##GPWS, Stall Warning
fq2 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'standard lib', 'p0') %>%
  set_database('p0: library flight safety events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 0: Event Information') %>%
  update_fieldtree('Profiles', 'Standard Library Profiles', 'P0: Library Flight Safety Events', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')

fq2 %>%
  save_metadata()


##Not in Takeoff Config
fq3 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p172: not in t/o config events') %>%
  set_database('p172: not in t/o config events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 172: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P172: Not in T/O Config', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P172: Not in T/O Config', 'Measured Items', 'Takeoff and Climb', 'Takeoff', 'CUSTOM')

fq3 %>%
  save_metadata()


##High Bank Angle
fq4 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p186: high bank angle profile events') %>%
  set_database('p186: high bank angle profile events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 186: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P186: High Bank Angle Profile', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P186: High Bank Angle Profile', 'Measured Items', 'Entire Flight (including taxi)', 'Measurements', 'Flight Dynamics', 'Roll', 'Measurements')

fq4 %>%
  save_metadata()


##Operating Altitude Exceedance
fq5 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p174: Maximum Operating Altitude Events') %>%
  set_database('p174: maximum operating altitude events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 174: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P174: Maximum Operating Altitude', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')

fq5 %>%
  save_metadata()


##HSRTO
fq6 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p196: HSRTO Events') %>%
  set_database('p196: HSRTO Events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 196: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P196: HSRTO', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')

fq6 %>%
  save_metadata()


##Inadvertent Spoiler Deployment
fq7 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p176: Inadvertant Spoiler Deployment Events') %>%
  set_database('p176: Inadvertant Spoiler Deployment Events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 176: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P176: Inadvertant Spoiler Deployment', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P176: Inadvertant Spoiler Deployment', 'Measured Items', 'Takeoff and Climb', 'Takeoff')

fq7 %>%
  save_metadata()


##Parking Brake in Flight
fq8 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p136: parking brake in flight events') %>%
  set_database('p136: parking brake in flight events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 136: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P136: Parking Brake in Flight', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')

fq8 %>%
  save_metadata()


##Egregious Unstable Approach
fq9 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'FOQA', 'Event Validation', 'P198: Unstable Approach Events') %>%
  set_database('p198: unstable approach events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 198: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'P198: unstable approach', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality') %>%
  update_fieldtree('Flight Review', 'Duplicate Detection') %>%
  update_fieldtree('Profiles', 'FOQA', 'Event Validation', 'p198: unstable approach', 'measured items',
                   'descent and landing', 'roll out', 'measurements', 'flight dynamics', 'vertical speed')

fq9 %>%
  save_metadata()


##Low Level Windshear
fq10 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p188: low level windshear events') %>%
  set_database('p188: low level windshear events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 188: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'event validation', 'P188: low level windshear', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')

fq10 %>%
  save_metadata()


##Low Fuel Landings
fq11 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p197: low fuel landings events') %>%
  set_database('p197: low fuel landings events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 197: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'event validation', 'P197: low fuel landings', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')

fq11 %>%
  save_metadata()


##Autopilot Disengaged in RVSM
fq12 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p216: autopilot disengage above rvsm events') %>%
  set_database('p216: autopilot disengage above rvsm events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 216: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'event validation', 'P216: autopilot disengage above rvsm', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')


fq12 %>%
  save_metadata()


##Ground Spoilers
fq13 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p205: ground spoilers not deployed events') %>%
  set_database('p205: ground spoilers not deployed events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 205: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'event validation', 'P205: ground spoilers not deployed', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')


fq13 %>%
  save_metadata()


##Flap Handle Movement during Takeoff
fq14 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p203: flap movement on takeoff events') %>%
  set_database('p203: flap movement on takeoff events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 203: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'event validation', 'P203: flap movement on takeoff', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')


fq14 %>%
  save_metadata()


##Low Energy (Airbus)
fq15 <- flt_query(conn, "ems4-app", metadata) %>%
  update_dbtree('apm events', 'foqa', 'event validation', 'p211: risk of stall (low energy) events') %>%
  set_database('p211: risk of stall (low energy) events - all events') %>%
  generate_preset_fieldtree() %>%
  update_fieldtree('Profile 211: Event Information') %>%
  update_fieldtree('Profiles', 'FOQA', 'event validation', 'P211: risk of stall (low energy)', 'Processing Status') %>%
  update_fieldtree('Flight Review', 'Data Quality')


fq15 %>%
  save_metadata()

}






