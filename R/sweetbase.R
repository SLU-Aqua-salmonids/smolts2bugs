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

#' Set or get limits for allowed values for some values
#'
#' @param ... Option names to retrieve option values or \code{[key]=[value]} pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' \itemize{
#'  \item{\code{root_folder}}{(\code{character};//storage-dh.slu.se/restricted$/Sötebasen/Exporter) Datafiles root }
#'  \item{\code{occations}}{(\code{character};SmoltfällaFångsttillfällen.csv) File with trapping occasions }
#'  \item{\code{catch_recatch}}{(\code{character};SmoltfällaÅterfångst.csv)  File with catch/recatch data}
#'  \item{\code{catch_envir}}{(\code{character};SmoltfällaTemperatur.csv) File with temp/waterlevel data}
#' }
#'
#' @export
sdb <- function(...){
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  OPTIONS(...)
}

## Data readers ##############################################################################################

#' Read smolt trapping occasion data
#'
#' Data with all smolt trappings defined with `AnstrTyp` = "Smoltfälla" are read
#' form a predefined Sötebasen export file. The location of the file is predefined
#' but can be changed, see ?sdb()
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
#'  }
#' @export
#'
#' @examples
#' \dontrun{
#' sdb(root_folder = ".") # Use current folder instead of the predefined
#' occasions <- sdb_read_occasions()
#' }
sdb_read_occasions <- function() {
  fp <- file.path(sdb()$root_folder, sdb()$occasions)
  res <- read.csv2(fp, fileEncoding = "latin1")
  return(res)
}
