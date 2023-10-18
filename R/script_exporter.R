#' Export a script
#'
#' The package have a couple of useful(?) scripts that you can use as a starting
#' point to build scripts that reformat data to data suitable for the BlackBox
#' bugs-model. Use `get_script()` to copy a script to your own workspace.
#'
#' Known scripts:
#' \itemize{
#'  \item{"söte_import2bugs.R"}{read and format a file in the Sötebasen import format to bugs}
#'  \item{"söte2bugs.R"}{read and format a file exported from Sötebasen to bugs}
#' }
#'
#' @param scriptname character Name of (known) script
#' @param outname character Name of output (default = scriptname)
#' @param overwrite logical Overwrite existing file (default = FALSE)
#'
#' @return
#' Invisible return the value of the file.copy() operation.
#' @export
#'
#' @examples
#' 2 + 2
#' \dontrun{
#' get_script("söte_import2bugs.R")
#' }
get_script <- function(scriptname, outname = scriptname, overwrite = FALSE) {
  known_names <- c("söte2bugs.R", "söte_import2bugs.R")
  if (!scriptname %in% known_names) {
    stop("Unknown script")
  }

    res <- file.copy(
      system.file("scripts", scriptname, package="smolts2bugs"),
      outname, overwrite = overwrite)
  invisible(res)
}
