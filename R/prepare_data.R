#' Create a matrix with all data for the Data2 matrix.
#'
#' Return a matrix with data almost ready to be cut and pasted into the
#' main data file (Data2_1group.odc) for the model.
#'
#' Each row data frame fish (with mandatory columns "marked", "capture_day" and "recapture_day") represent
#' a caught fish. Column capture_day contains the number of days between the start of the experiment
#' the day the fish was caught for the first time. Column recapture_day is the same if recaptured
#' and NA otherwise. Column marked is set to TRUE for all fish that was marked and subject
#' for for recapture and FALSE otherwise, e.g. when marked is false the fish was not marked
#' and not subject for possible recapture.
#'
#' Data frame env must have ndays rows and two mandatory columns w_temp (water temperature)
#' and w_level (water level). No NAs allowed so impute the data if you have missing values.
#'
#' Ndays is the number of days the experiment was running including any downtime.
#' Optional parameter missing_dats is a vector with day numbers when the experiment was put
#' on hiatus.
#'
#' @param fish data frame with three mandatory columns "marked", "capture_day" and "recapture_day"
#' @param env data frame with two mandatory columns "w_temp" and "w_level".
#' @param ndays numeric number of days data have in operation, e.g. start date to stop date
#' @param missing_days numeric vector of day numbers when capture (and recapture) didn't work.
#'
#' @return a matrix with ndays rows and ndays + 6 columns
#' @export
#'
#' @examples
#' ndays <- 5
#' (smolts <- data.frame(
#'   capture_day = c(2,2,3),
#'   recapture_day = c(NA, 3, 4),
#'   marked = c(FALSE, TRUE, TRUE)))
#' (envdata <- data.frame(
#'   w_temp = c(7, 7, 8, 8.5, 9),
#'   w_level = c(100, 110, 105, 150, 145)))
#'
#' format_Data2(fish = smolts, env = envdata, ndays = ndays)
#'
format_Data2 <- function(fish, env, ndays, missing_days = NULL) {
  if (!all(c("capture_day", "recapture_day", "marked") %in% colnames(fish))) {
    stop("data fish must have columns marked, capture_day and recapture_day")
  }
  if (ndays != nrow(env)){
    stop("number of rows in env must be equal to ndays")
  }
  if (!all(c("w_temp", "w_level") %in% colnames(env))) {
    stop("data frame env must have columns w_temp and w_level")
  }

  R <- array(0, dim = c(ndays, ndays)) # Recaptures for day caught
  M <- array(0, dim = c(ndays))          # Marked per day
  C <- array(0, dim = c(ndays))          # captured per day
  Z <- array(0, dim = c(ndays))          # ?
  N <- array(0, dim = c(ndays))          # ?
  #
  # Loop through all fish and increase the array cell for the day indicated
  # by capture_day and recapture_day.
  for (rownum in 1:nrow(fish)) {
    i <- fish$capture_day[rownum]
    j <- fish$recapture_day[rownum]
    if (fish$marked[rownum]) { # If the fish is marked increase both C and M, unmarked only C
      M[i] <- M[i] + 1
      C[i] <- C[i] + 1
    } else {
      C[i] <- C[i] + 1
    }
    if (!is.na(j)) {
      R[i, j] <- R[i, j] + 1
    }
  }
  # If user provided  missing days set number of catch (C) and
  # number of recaptures (R) to -9 on those days.
  # User must replace -9 with the string "NA". If package writexl can get an
  # option to write NAs (like showNA in package xlsx) we will change this to
  # NA instead of -9.
  if (!is.null(missing_days)) {
    C[missing_days] <- -9
    for (mday in missing_days) {
      R[mday, mday] <- -9
    }
  }
  # Put everything together in one array with colnames in "Blackbox" format
  colnames(R) <- sprintf("r[,%d,1]", seq_len(ndays))
  bugs_data <- cbind('m[,1]' = M, 'c[,1]' = C, R,
                     'wt[,1]' = env$w_temp, 'wl[,1]' = env$w_level,
                     'z[,1]' = 0, 'n[,1]' = 0)
  return(bugs_data)
}

#' Generate Inits2 data
#'
#' Generate a matrix suitable to save into a Inits2cX_group1.odc file. This function
#' is used by save_bugsdata() and is probably not called directly by the user.
#'
#' @param n numeric number of values generated
#' @param llambda numeric value(s) that will be repeated to get n values (default .1)
#' @param lphi numeric value(s) that will be repeated to get n values (default 1)
#' @param g numeric value(s) that will be repeated to get n values
#'
#' @return numeric matrix n x 3 with column names 'llambda[,1]', 'lphi[,1]' and 'g[,1]'
#' @export
#'
#' @examples
#' \dontrun{
#' gen_Inits2(3, g = g_inits_1)
#' }
#'
gen_Inits2 <- function(n, llambda = .1 , lphi = 1, g) {
  res <- cbind('llambda[,1]' = rep_len(llambda, n),
               'lphi[,1]' = rep_len(lphi, n),
               'g[,1]' = rep_len(g, n)) # sample(g[g > 0], n, replace = TRUE)
  return(res)
}

#' Save an Excel file with Bugs data
#'
#' This function will save an Excel file with all data needed to create all
#' files needed to run the smolt model in BlackBox. The file will have 6 sheets
#' named Data1, Data2, Inits1c1, Inits1c2, Inits2c1 and Inits2c2. Each sheet corresponds
#' to an .odc file in BlackBox and you must cut'n'paste from one format to the other.
#'
#' @param x numeric matrix for Data2 file. See format_Data2()
#' @param path character path to create file in. Default current working directory
#' @param fname character file name of created file. Default "bugsdata.xlsx"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' save_bugsdata(Data2)
#' }
#'
save_bugsdata <- function(x, path = ".", fname = "bugsdata.xlsx") {
  ndays <- nrow(x)
  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  Data1 <- data.frame(Data1 = sprintf("list(N=c(%d),w=c(1))", ndays))
  Data2 <- as.data.frame(x)
  Inits1c1 <- data.frame(
    Inits1c1 = "list(nu0=c(-2),omega1=c(0),omega2=c(0),psi1=c(0) ,psi2=c(0),psi0=c(1),nu1=c(0),nu2=c(0),logU=c(9.2),sigma=c(29),xi=c(1),omega0=c(-1),rho=c(1),pi=c(1))") #file.path(path, "Inits1c1_1group.txt")
  Inits1c2 <- data.frame(
    Inits1c2 = "list(nu0=c(-2.1),omega1=c(0.1),omega2=c(-0.1),psi1=c(-0.1) ,psi2=c(0.1),psi0=c(-0.1),omega0=c(0.1),pi=c(0.5),rho=c(1.1),nu1=c(0.1),nu2=c(-0.1),logU=c(9.5),sigma=c(30))") #file.path(path, "Inits1c2_1group.txt")
  ##
  # Create Initsc2c1. llambda .1, lphi 1, and g that we take from old data
  Inits2c1 <- as.data.frame(gen_Inits2(ndays, llambda = .1, lphi = 1, g =  g_inits_1))
  Inits2c2 <- as.data.frame(gen_Inits2(ndays, llambda = .1, lphi = 1.5, g = g_inits_2))
  ##
  writexl::write_xlsx(list(Data1 = Data1, Data2 = Data2,
                  Inits1c1 = Inits1c1, Inits1c2 = Inits1c2,
                  Inits2c1 = Inits2c1, Inits2c2 = Inits2c2),
             path = file.path(path, fname))
}

#' Save bugsdata + some meta data as a R dump
#'
#' This function will save a R data dump with data needed for the smolt model + meta data
#' describing the mark/recapture experiment. This file can be loaded to analyze the data,
#' generate reports etc...
#'
#'
#' @param x numeric matrix for Data2 file. See format_Data2()
#' @param river character name of the river the data was collected
#' @param species character name of species
#' @param startd Date first date trap was running
#' @param stopd Date last date trap was running
#' @param missing_days numeric vector of day numbers when capture (and recapture) didn't work.
#' @param path character path to create file in. Default current working directory
#' @param fname character file name of created file. Default "RData_dump.RData"
#'
#' @export
#'
save_Rdatadump <- function( x, river, species, startd, stopd, missing_days = NULL,
                            path = ".", fname = "RData_dump.RData") {
  rdata <- list(
    "river" = river,
    "species" = species,
    "startd" = startd,
    "stopd" = stopd,
    "missing_days" = missing_days,
    "N"=nrow(x),
    #   "w"=c(1),
    "m"  = x[ , 1],
    "c"  = x[ , 2],
    "r"  = x[ , 3:(ncol(x)-4)],
    "wt" = x[ , (ncol(x)-3)],
    "wl" = x[ , (ncol(x)-2)],
    "z"  = x[ , (ncol(x)-1)],
    "n"  = x[ , (ncol(x))])
  rdata$c <- ifelse(rdata$c == -9, as.integer(NA), rdata$c)
  rdata$r <- ifelse(rdata$r == -9, as.integer(NA), rdata$r)
  if (!file.exists(path)) {
    dir.create(path)
  }
  RData_dump <- file.path(path, fname)
  save(rdata, file = RData_dump)
}


#' Convert dates to day numbers
#'
#' A vector of dates (or data that can be coerced as dates) are converted
#' to day number relative to start_date. E.g. if a date is equal to start_date
#' 1 is returned.
#'
#' @param start_date object that can be coerced with as.Date
#' @param date_list  vector that can be coerced with as.Date
#'
#' @return vector of integers
#' @export
#'
#' @examples
date_number <- function(start_date, date_list) {
  res <-as.numeric(as.Date(date_list) - as.Date(start_date) + 1)
  return(res)
}
