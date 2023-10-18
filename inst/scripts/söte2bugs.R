###
## Script to transform a smoltdata exported from Sötebasen into formats usable
## as input to mark/recapture models.
##  Time-stamp: <2023-10-18 12:50:06 ankag>
##

library(dplyr)
library(readxl)
library(writexl)
library(lubridate)
library(Smoltreg)
library(smolts2bugs)
##
sweet_file <- file.choose() # Get filenamew interactivly
#sweet_file <- "c:/ExportSötebasen/AA02_MarkRecapSmolts_Query.xlsx" # Use hardcoded name
d <- read_excel(sweet_file)
river <- d$VattenNamn[1]
year <- year(d$AnstrDatumStart[1])
## Choose species here
species <- "Lax"
#species <- "Öring"

RESULTDIR <- paste0("SMOLTS2_", river, "_", year)
SPECIESDIR <- file.path(RESULTDIR, species)
minlength <- Smoltreg_limits()$minlength
maxlength <- Smoltreg_limits()$maxlength
## dates <- read_excel(sweet_file, sheet = "Ansträngning") %>%
dates <- d[1, ] %>%
  mutate(N_days = 1 + as.numeric(AnstrDatumSlut - AnstrDatumStart),
         start_day_of_year = as.POSIXlt(AnstrDatumStart)$yday,
         start_date = AnstrDatumStart,
         stop_date = AnstrDatumSlut) %>%
  select(N_days, start_day_of_year, start_date, stop_date)


# Get all rows for a species and remove too small and too fish
#fish <- read_excel(sweet_file, sheet = "Individ") %>%
fish <- d %>%
  filter(Art == species) %>%
  filter(between(Längd1, minlength, maxlength) | is.na(Längd1)) %>%
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
####
## Create a data frame "tagged" with all fish caught, marked and relased
## e.g. all fish in "" tagged are  subject for recapture.
tagged <- fish %>% filter(event == Smoltreg::event$MARKED) %>%
  select(pittag, day_of_year, species) %>%
  rename(capture_day = day_of_year) %>%
    mutate(capture_day = capture_day - dates$start_day_of_year + 1)
####
## Create a data frame "recaptured" with all recaptures
recaptured <- fish %>% filter(event == Smoltreg::event$RECAPTURED) %>%
  select(pittag, day_of_year) %>%
  rename(recapture_day = day_of_year) %>%
  mutate(recapture_day = recapture_day - dates$start_day_of_year + 1)

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

all_captures <- tagged %>%
    left_join(recaptured, by = "pittag") %>%
    mutate(marked = TRUE) %>%
    bind_rows(captured)
####
## Read water temp and water level.
#envfile <- file.choose()
envfile <- "c:/ExportSÃ¶tebasen/AA03_Temperatur_Query.xlsx"
#envdata <- read_excel(sweet_file, sheet = "Temperatur") %>%
envdata <- read_excel(envfile) %>%
  mutate(dnum = as.POSIXlt(Datum)$yday, date = Datum,
         w_level = WL, w_temp = WT) %>%
  select(dnum, date, w_level, w_temp) %>%
    filter(between(date, dates$start_date, dates$stop_date)) # Remove measures out
#####
## TODO. Set missing days. Missing dates aren't in SÃ¶tebasen (yet?).
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
Data2  <- format_Data2(fish=all_captures, env=envdata, ndays=dates$N_days,
                       missing_days = missing_days)
save_bugsdata(Data2, path = SPECIESDIR)
save_Rdatadump(Data2, river = river, species = species,
               startd = dates$start_date, stopd = dates$stop_date,
               missing_days = missing_days, path = SPECIESDIR)

