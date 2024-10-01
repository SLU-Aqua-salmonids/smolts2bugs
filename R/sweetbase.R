##
## This file contains code needed to read and process the smoltdata stored in Sötebasen
## The data is read from data that are regulary exported from Sötebasen in a
## format we have designed.
##

## Sötebasen options ##################################################################################
# Variable, global to package's namespace.
# This function is not exported to user space and does not need to be documented.
OPTIONS <- settings::options_manager(
  root_folder = "//storage-dh.slu.se/restricted$/Sötebasen/Exporter",
  occasions = "SmoltfällaFångsttillfällen.csv",
  catch_recatch = "SmoltfällaÅterfångst.csv",
  catch_envir = "SmoltfällaTemperatur.csv")



# User function that gets exported:

#' Set or get values for options related to Sötebasen.
#'
#' Folder and filenames used to find data exported from Sötebasen on regular
#' basis. Default values points to locations on the Sölab restricted servers.
#' Change needed if you want to work on private copies of the data, for example
#' if you work remote without VPN.
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{root_folder} Datafiles root (//storage-dh.slu.se/restricted$/Sötebasen/Exporter)}
#'  \item{\code{occations} File with trapping occasions (SmoltfällaFångsttillfällen.csv)}
#'  \item{\code{catch_recatch} File with catch/recatch data (SmoltfällaÅterfångst.csv)}
#'  \item{\code{catch_envir}  File with temp/waterlevel data (SmoltfällaTemperatur.csv)}
#' }
#'
#' @export
sdb <- function(...){
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  OPTIONS(...)
}

## Data readers ##############################################################################################

#' Read smolt trapping occasion data from Sötebasen export
#'
#' Data with all smolt trappings defined with `AnstrTyp` = "Smoltfälla" are read
#' from a predefined Sötebasen export file. The location of the file is predefined
#' but can be changed, see ?sdb()
#'
#' @param VattenNamn return data only from river = VattenNamn, optional
#' @param Year return data only from Year, optional
#'
#' @return
#' Return a data frame with columns:
#' \itemize{
#'  \item{InsamlingID}{}
#'  \item{AnsträngningID}{}
#'  \item{AnstrDatumStart}{}
#'  \item{AnstrDatumSlut}{}
#'  \item{AnstrTyp}{}
#'  \item{VattenNamn}{}
#'  \item{HaroID}{}
#'  \item{AnstrPlats}{}
#'  \item{Year}{}
#'  }
#' @export
#'
#' @examples
#' \dontrun{
#' sdb(root_folder = ".") # Use current folder instead of the predefined
#' occasions <- sdb_read_occasions()
#' }
sdb_read_occasions <- function(VattenNamn = NULL, Year = NULL) {
  fp <- file.path(sdb()$root_folder, sdb()$occasions)
  res <- read.csv2(fp, fileEncoding = "latin1")
  res <- res %>%
    dplyr::mutate(AnstrDatumStart = as.Date(AnstrDatumStart),
           AnstrDatumSlut = as.Date(AnstrDatumSlut),
           Year = lubridate::year(AnstrDatumStart)
    )
  if (!is.null(VattenNamn)) {
    vn <- VattenNamn
    res <- res %>% filter(VattenNamn %in% vn)
  }
  if (!is.null(Year)) {
    y <- Year
    res <- res %>% filter(Year %in% y)
  }
  return(res)
}

#' Read smolt catch/recatch data from Sötebasen export
#'
#' Data with all smolt catches defined with `AnstrTyp` = "Smoltfälla" are read
#' from a predefined Sötebasen export file. The location of the file is predefined
#' but can be changed, see ?sdb()
#'
#' @param Art return data only from Art = Art, optional
#' @param VattenNamn return data only from river = VattenNamn, optional
#' @param Year return data only from Year, optional
#'
#' @return
#' Return a data frame with columns:
#' \itemize{
#'  \item{InsamlingID}{}
#'  \item{AnsträngningID}{}
#'  \item{FångstDatum}{}
#'  \item{FångstTid}{}
#'  \item{Art}{}
#'  \item{Längd1}{}
#'  \item{Vikt1}{}
#'  \item{Behandling}{}
#'  \item{MärkeNr}{}
#'  \item{StadiumKod}{}
#'  \item{AnmIndivid}{}
#'  \item{VattenNamn}{}
#'  \item{AnstrPlats}{}
#'  \item{Year}{}
#'  }
#' @export
#'
#' @examples
#' \dontrun{
#' sdb(root_folder = ".") # Use current folder instead of the predefined
#' catch <- sdb_read_catch_recatch()
#' }
sdb_read_catch_recatch <- function(Art = NULL, VattenNamn = NULL, Year = NULL) {
  fp <- file.path(sdb()$root_folder, sdb()$catch_recatch)
  res <- read.csv2(fp, fileEncoding = "latin1")
  res <- res %>%
    dplyr::mutate(FångstDatum = as.Date(FångstDatum),
                  Year = lubridate::year(FångstDatum)
    )
  if (!is.null(Art)) {
    res <- res[res$Art == Art,]
  }
  if (!is.null(VattenNamn) | !is.null(Year)) {
    sel_ID <- sdb_read_occasions(VattenNamn = VattenNamn, Year = Year) %>%
      dplyr::select(InsamlingID, AnsträngningID) %>%
      dplyr::distinct()
    res <- dplyr::left_join(sel_ID, res, by = dplyr::join_by(InsamlingID, AnsträngningID))

  }
  return(res)
}

#' Read smolt trap environment data from Sötebasen export
#'
#' Data with all environment variables collected during trapping (`AnstrTyp` = "Smoltfälla") are read
#' from a predefined Sötebasen export file. The location of the file is predefined
#' but can be changed, see ?sdb()
#'
#' @return
#' Return a data frame with columns:
#' \itemize{
#'  \item{InsamlingID}{}
#'  \item{AnstrångningID}{}
#'  \item{Datum}{}
#'  \item{WT}{}
#'  \item{WL}{}
#'  \item{Year}{}
#'  }
#' @export
#'
#' @examples
#' \dontrun{
#' sdb(root_folder = ".") # Use current folder instead of the predefined
#' catch <- sdb_read_catch_recatch()
#' }
sdb_read_catch_envir <- function(VattenNamn = NULL, Year = NULL) {
  fp <- file.path(sdb()$root_folder, sdb()$catch_envir)
  res <- read.csv2(fp, fileEncoding = "latin1")
  res <- res %>%
    dplyr::mutate(Datum = as.Date(Datum),
                  Year = lubridate::year(Datum)
    )
  if (!is.null(VattenNamn) | !is.null(Year)) {
    sel_ID <- sdb_read_occasions(VattenNamn = VattenNamn, Year = Year) %>%
      dplyr::select(InsamlingID, AnsträngningID) %>%
      dplyr::distinct()
    res <- dplyr::left_join(sel_ID, res, by = dplyr::join_by(InsamlingID, AnsträngningID))

  }
  return(res)
}
