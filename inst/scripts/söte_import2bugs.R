###
## Script to transform a Sötebasen import-file into formats usable
## as input to mark/recapture models.
##  Time-stamp: <2023-04-26 12:50:06 ankag>
##
library(dplyr)
library(readxl)
library(writexl)
library(imputeTS)
library(Smoltreg)
library(smolts2bugs)
##
sweet_file <- file.choose()
#sweet_file <- "../Testeboån/2021/smoltreg_Testeboån_2021_sötebasen.xlsx"
i <- read_excel(sweet_file, sheet = "Insamling")
river <- i$Vatten
year <- i$Årtal
species <- "Lax"
#species <- "Öring"

RESULTDIR <- paste0("SMOLTS_", river, "_", year)
SPECIESDIR <- file.path(RESULTDIR, species)
minlength <- Smoltreg_limits()$minlength
maxlength <- Smoltreg_limits()$maxlength
#minlength <- 129
#maxlength <- 300
dates <- read_excel(sweet_file, sheet = "Ansträngning") %>%
  mutate(N_days = 1 + as.numeric(AnstrDatumSlut - AnstrDatumStart),
         start_day_of_year = as.POSIXlt(AnstrDatumStart)$yday,
         start_date = AnstrDatumStart,
         stop_date = AnstrDatumSlut) %>%
  select(N_days, start_day_of_year, start_date, stop_date)


# Get all rows for a species and remove too small and too long fish
fish <- read_excel(sweet_file, sheet = "Individ") %>%
  filter(Art == species) %>%
  filter(between(Längd1, minlength, maxlength) | is.na(Längd1)) %>%
  mutate(day_of_year = as.POSIXlt(FångstDatum)$yday, pittag = MärkeNr,
         event = case_when(Behandling == "Utsatt" ~ Smoltreg::event$CAUGHT,
                           Behandling == "Märkt&utsatt" ~ Smoltreg::event$MARKED,
                           Behandling == "Återfångad&utsatt" ~ Smoltreg::event$RECAPTURED,
                           Behandling == "Landad/avlivad/död" ~ Smoltreg::event$REMOVED,
                           TRUE ~ Smoltreg::event$UNKNOWN)) %>%
  select(pittag = MärkeNr, day_of_year, length = Längd1, event, species = Art)


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
envdata <- read_excel(sweet_file, sheet = "Temperatur") %>%
    mutate(dnum = as.POSIXlt(MätDatum)$yday, date = as.Date(MätDatum),
           w_level = Vattennivå, w_temp = Tempbotten) %>%
    select(dnum, date, w_level, w_temp) %>%
    mutate(w_level = imputeTS::na_interpolation(w_level)) %>%
    mutate(w_temp = imputeTS::na_interpolation(w_temp)) %>%
    filter(between(date, dates$start_date, dates$stop_date)) # Remove measures out
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
## Use smolts2bugs::format_Data2 to format our data as a matrix almost ready to
## be used as Data2 in the Blackbox smolt model.
## Save the formated data in excel-file and also in an Rdump file.
## The Rdump contains some useful metadata besides the Data2 structure.
Data2  <- format_Data2(fish=all_captures, env=envdata, ndays=dates$N_days,
                       missing_days = missing_days)
save_bugsdata(Data2, path = SPECIESDIR)
save_Rdatadump(Data2, river = river, species = species,
               startd = dates$start_date, stopd = dates$stop_date,
               missing_days = missing_days,path = SPECIESDIR)

