###
## Script to transform a smoltdata exported from Sötebasen into formats usable
## as input to mark/recapture models.
## This file is a part of package smolts2bugs, see smolts2bugs::get_script
##

## TODO:
## The two scripts sote2bugs.R and sote_import2bugs.R share a lot of coded and
## should be merged to one script where different input methods can be choosen.
##

library(dplyr)
library(readxl)
library(writexl)
library(lubridate)
library(Smoltreg) # https://github.com/SLU-Aqua-diadromous/Smoltreg.git
library(smolts2bugs) # https://github.com/SLU-Aqua-diadromous/smolts2bugs.git
###
## Choose river, year and species to process
river <- "Ljungan"
year <- year(Sys.Date())
species <- "Lax"
#species <- "Öring"
###
## Location input files are set with smolts2bugs::sdb(). By default files
## automatically exported to Sölab file server. See ?sdb for options.
## E.g. to read files from current current directory (instead of default)
## run sdb(root_folder = ".")

RESULTDIR <- paste0("SMOLTS_", river, "_", year)
SPECIESDIR <- file.path(RESULTDIR, species)
message(paste0("Results will be saved in folder: ", SPECIESDIR))
minlength <- Smoltreg_limits()$minlength
maxlength <- Smoltreg_limits()$maxlength

## Get all rows for a species
## Create a filter to select all fish valid for this mark/recapture study.
## Two examples are in the code below. Option one is to only use fish where
## Längd1 is between minlength and maxlength (need to include is.na(Längd1) to get recaptures).
## This is the variant we have used for all estimates up to 2022.
## In 2023 in Ljungan we tried to use StadiumKod c("", "S1", "S2", "S3"). E.g.
## all StadiumKod that are smolt or smolt-like also include recaptures.
fish <- sdb_read_catch_recatch(Art = species, VattenNamn = river, Year = year) %>%
  filter(between(Längd1, minlength, maxlength) | is.na(Längd1))
#    filter(StadiumKod %in% c("S1", "S2", "S3") | Behandling == "Återfångad&utsatt")
message("First catch: ", min(fish$FångstDatum), " Last catch: ", max(fish$FångstDatum))

## Reformat, recode and keep only relevant columns
fish <- fish %>%
  mutate(day_of_year = as.POSIXlt(FångstDatum)$yday, pittag = MärkeNr,
         event = case_when(Behandling == "Utsatt" ~ Smoltreg::event$CAUGHT,
                           Behandling == "Märkt&utsatt" ~ Smoltreg::event$MARKED,
                           Behandling == "Återfångad&utsatt" ~ Smoltreg::event$RECAPTURED,
                           Behandling == "Landad/avlivad/död" ~ Smoltreg::event$REMOVED,
                           TRUE ~ Smoltreg::event$UNKNOWN)) %>%
  select(pittag, day_of_year, length = Längd1, event, species = Art)

if (any(fish$event == Smoltreg::event$UNKNOWN)) {
  stop("Unknown event in fish. PLEASE FIX!!")
}

#fish <- fish %>% filter(!(is.na(length) & event == CAUGHT)) # Åby special

## Read the dates (period) for this trapping. Here you have the option to
## shorten the start and stop if we have many leading or trailing dates with
## with zero catch.
dates <-  sdb_read_occasions(VattenNamn = river, Year = year)
message("AnstrDatumStart: ", min(dates$AnstrDatumStart), " AnstrDatumSlut: ", max(dates$AnstrDatumSlut))

## Reformat, recode and keep only relevant columns, optionally adjust dates.
dates <- dates %>%
#  mutate(AnstrDatumStart = as.Date("2023-05-11")) %>%
#  mutate(AnstrDatumSlut = as.Date("2023-06-22")) %>% # Don't use long period of leading and trailing zero catch
  mutate(N_days = 1 + as.numeric(AnstrDatumSlut - AnstrDatumStart),
         start_day_of_year = as.POSIXlt(AnstrDatumStart)$yday,
         start_date = AnstrDatumStart,
         stop_date = AnstrDatumSlut) %>%
  select(N_days, start_day_of_year, start_date, stop_date)

####
## Create a data frame "tagged" with all fish caught, marked and relased
## e.g. all fish in "" tagged are  subject for recapture.
tagged <- fish %>% filter(event == Smoltreg::event$MARKED) %>%
  select(pittag, day_of_year, species) %>%
  rename(capture_day = day_of_year) %>%
    mutate(capture_day = capture_day - dates$start_day_of_year + 1)

####
## Create a data frame "captured" with fish that was captured but NOT
## subject for recapture, e.g. fish that died or was released downstream
## the trap.
captured <- fish %>%
##  filter(event == CAUGHT | event == UNKNOWN) %>%  #Special Laxens hus 2019. Märkta i Ekeberg markerade UNKNOWN
    filter(event == Smoltreg::event$CAUGHT | event == Smoltreg::event$REMOVED) %>%
    select(pittag, day_of_year, species) %>%
    rename(capture_day = day_of_year) %>%
    mutate(pittag = as.character(NA),
           capture_day = capture_day - dates$start_day_of_year + 1,
           recapture_day = NA,
           marked = FALSE)
####
## Create a data frame "recaptured" with all recaptures
recaptured <- fish %>% filter(event == Smoltreg::event$RECAPTURED) %>%
  select(pittag, day_of_year) %>%
  rename(recapture_day = day_of_year) %>%
  mutate(recapture_day = recapture_day - dates$start_day_of_year + 1)

all_captures <- tagged %>%
    left_join(recaptured, by = "pittag") %>%
    mutate(marked = TRUE) %>%
    bind_rows(captured)
####
## Read water temp and water level.
envdata <- sdb_read_catch_envir(VattenNamn = river, Year = year) %>%
  mutate(dnum = as.POSIXlt(Datum)$yday, date = Datum,
         w_level = WL, w_temp = WT) %>%
  select(dnum, date, w_level, w_temp) %>%
    filter(between(date, dates$start_date, dates$stop_date)) # Only keep dates within this trapping period
#####
## TODO. Set missing days. Missing dates aren't in Sötebasen (yet?).
## See tab Metadata in the Smoltreg-file if we have missing dates and enter them
## below.
## Missing days should be entered as day number where dates$start_date = 1
##
## If no days missing Set to NULL.
missing_days  <- NULL
#missing_days  <- c(6)
####
## Use smolts2bugs::format_Data2 to our data as a matrix almost ready to
## be used as Data2 in the Blackbox smolt model.
## Save the formated data in excel-file and also in an Rdump file.
## The Rdump contains some useful metadata besides the Data2 structure.
Data2  <- smolts2bugs::format_Data2(fish=all_captures, env=envdata, ndays=dates$N_days,
                       missing_days = missing_days)
smolts2bugs::save_bugsdata(Data2, path = SPECIESDIR)
smolts2bugs::save_Rdatadump(Data2, river = river, species = species,
               startd = dates$start_date, stopd = dates$stop_date,
               missing_days = missing_days, path = SPECIESDIR)

## Save a draft of the reults.Rmd template in the output directory. When models
## have produced rresults knit this draft to get a report.
if (require(SmoltReports)) {
  fname <- paste0("results_", river, "_", species, "_", year, ".Rmd")
  rmarkdown::draft(file.path(SPECIESDIR, fname),
                   template = "smolt-estimates-blackbox",
                   package = "SmoltReports", edit = FALSE)
}
