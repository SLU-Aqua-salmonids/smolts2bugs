#' Export a script
#'
#' The package have a couple of useful(?) scripts that you can use as a starting
#' point to build scripts that reformat data to data suitable for the BlackBox
#' bugs-model. Use `get_script()` to copy a script to your own workspace.
#'
#' Known scripts:
#' \itemize{
#'  \item{"sote_import2bugs.R"} {read and format a file in the Sötebasen import format
#'  (i.e. the format we submit to Sötebasen admin for import) to bugs}
#'  \item{"sote2bugs.R"} {read and format a file exported from Sötebasen (i.e. a file
#'  we get when we export data from Sötebasen) to bugs}
#' }
#'
#' @param scriptname character Name of (known) script
#' @param outname character Name of output (default = scriptname)
#' @param overwrite logical Overwrite existing file (default = FALSE)
#'
#' @return
#' Invisible return the value of the file.copy() operation. As a side effect
#' a script file is saved
#' @export
#'
#' @examples
#' \dontrun{
#' get_script("sote_import2bugs.R")
#' }
get_script <- function(scriptname, outname = scriptname, overwrite = FALSE) {
  known_names <- c("sote2bugs.R", "sote_import2bugs.R")
  if (!scriptname %in% known_names) {
    kn <- paste0(known_names, collapse = ", " )
    stop(paste0("Unknown script. Use one of ", kn))
  }

    res <- file.copy(
      system.file("scripts", scriptname, package="smolts2bugs"),
      outname, overwrite = overwrite)
  invisible(res)
}
