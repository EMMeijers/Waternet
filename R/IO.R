# IO functions:


#' convert SOBEK <.map> or <.his> file into an R array object,
#'
#' @param filename the <.his> or <.map> file to be converted.
#' @return An R array object of the SOBEK <.his> or <.map> file named  \code{filename}.
#' @examples
#' library(Waternet)
#' arr <- sobek2arr(filename = "DATA/testdata.his")
#' submod <- c("OXY", "Cl")
#' locmod <- c("LOX003","LOX009")
#' df <- arr2df(arr, locmod=locmod, submod=submod)
#' library(ggplot2)
#' plot <- ggplot(df, aes(time, value)) +
#'   geom_line(aes(color = variable), size = 1) +
#'   facet_grid((variable ~ location), scales = "free")
#' plot
sobek2arr <- function (filename, timestamp = T, begintime = "1900-01-01 00:00:00"){
  
  `%not_in%` <- Negate(`%in%`)
  
  fn.ext <- toupper(substr(filename, nchar(filename) - 3, nchar(filename)))
  library("stringr")
  if (fn.ext %not_in% c(".HIS", ".MAP")) { 
    stop("filename does not seem to be a <.his> of <.map> file")
  }
  zz <- file(filename, "rb")
  readChar(zz, 40)
  readChar(zz, 40)
  readChar(zz, 40)
  readChar(zz, 4)
  timeorigin <- readChar(zz, 19)
  readChar(zz, 7)
  scu.prep <- readChar(zz, 8)
  scu  <- as.numeric(scu.prep) # check for the internal timer
  sign.scu <- sign(scu)  # sign can be +1, 0 or -1
  
  ifelse(is.na(sign.scu), dec.sign.scu <- NA,   # NB. no error handling here yet, NA should only occur if timestamp = F and then you don't need dec.sign.scu
         ifelse(sign.scu == 1 , dec.sign.scu <- "*",
                ifelse(sign.scu == -1, dec.sign.scu <- "/",
                       ifelse(sign.scu == 0, stop("The sign of your internal timer is neither negative nor positive, but 0."), stop("Check dec.sign.scu.")))))
  
  scu.sym  <- readChar(zz, 1)
  readChar(zz, 1)
  afm <- readBin(zz, integer(), n = 2)
  
  syname <- vector("character", afm[1])
  for (i in 1:afm[1]) {
    syname[i] <- readChar(zz, 20)
  }
  # Read locations names, different handling of <.HIS> and <.MAP> file:
  if (fn.ext == ".HIS") {
    idump <- vector("integer", afm[2])
    duname <- vector("integer", afm[2])
    for (i in 1:afm[2]) {
      idump[i] <- readBin(zz, integer(), n = 1)
      duname[i] <- readChar(zz, 20)
    }  
  } else {
    idump <- seq(1:afm[2])
  }
  
  
  loc <- seek(zz)
  it <- -1
  itn <- vector("integer", 0)
  tel <- 0
  while (length(it) > 0) {
    tel <- tel + 1
    it <- readBin(zz, integer(), n = 1)
    if (length(it) > 0) {
      itn <- c(itn, it)
      conc <- readBin(zz, "double", n = afm[1] * afm[2],
                      size = 4)
      length(conc)
    }
  }
  seek(zz, where = loc)
  concar <- array(dim = c(length(itn), afm[2], afm[1]))
  for (i in 1:length(itn)) {
    it <- readBin(zz, integer(), n = 1)
    concar[i, , ] <- matrix(readBin(zz, "double", n = afm[1] * afm[2], size = 4), nrow = afm[2], ncol = afm[1], byrow = T)
  }
  close(zz)
  timeorigin <- str_replace_all(timeorigin, "[.]", "-")
  
  
  ifelse(timestamp, itn2 <- as.character(as.POSIXct(x = sapply(itn, function(x) as.numeric(eval(parse(text = paste(x, dec.sign.scu, scu))))),
                                                    origin = timeorigin, tz = "GMT")), itn2 <- as.character(as.POSIXct(x = as.character(as.POSIXct(x = as.numeric(itn),
                                                                                                                                                   origin = begintime, tz = "GMT")))))
  if (fn.ext == ".HIS") {
    dimnames(concar) <- list(itn2, str_trim(duname), str_trim(syname))
  } else {
    dimnames(concar) <- list(itn2, str_trim(idump), str_trim(syname))
  }
  return(concar)
}

#' Get locations from data array,
#'
#' @param 'data object (array).
#' @return list of locations in data
get_data_locs <- function(data) {
  locs <- attr(data, 'dimnames')[[2]] ## locations
  return(locs)
}

#' Get varabeles from data array,
#'
#' @param 'data object (array).
#' @return list of variables in data
get_data_vars <- function(data) {
  vars <- attr(data, 'dimnames')[[3]] ## variables
  return(vars)
}

#' Get times from data array,
#'
#' @param 'data object (array).
#' @return list of times in data
get_data_tims <- function(data) {
  tims <- attr(data, 'dimnames')[[1]] ## times
  return(tims)
}
#' extract data from array into a dataframe for selected locations and substances,
#'
#' @param arr the array to be extracted.
#' @param locmod the locations in the array to be extracted
#' @param submod the substances in the array to be extracted
#' @return A dataframe with model output values for \code{submod} and \code{locmod}.
#' @examples
#' library(Waternet)
#' arr <- delwaq2arr(filename = "DATA/testdata.his")
#' submod <- c("OXY", "Cl")
#' locmod <- c("LOX003","LOX009")
#' df <- arr2df(arr, locmod=locmod, submod=submod)
#' library(ggplot2)
#' plot <- ggplot(df, aes(time, value)) +
#'   geom_line(aes(color = variable), size = 1) +
#'   facet_grid((variable ~ location), scales = "free")
#' plot
arr2df <- function(arr, locmod, submod) {
  require(reshape2)
  
  if(length(submod) != 1 & length(locmod) != 1) {
    df.mod <- melt(arr[, locmod, submod], varnames=c("time", "location", "variable"))
  }
  if(length(submod) == 1 & length(locmod) != 1) {
    df.mod <- melt(arr[, locmod, submod], varnames=c("time", "location"))
    df.mod$variable <- submod
  }
  if(length(locmod) == 1 & length(submod) != 1) {
    df.mod <- melt(arr[, locmod, submod], varnames=c("time", "variable"))
    df.mod$location <- locmod
  }
  if(length(locmod) == 1 & length(submod) == 1) {
    df.mod <- melt(arr[, locmod, submod], varnames=c("time"))
    df.mod$location <- locmod
    df.mod$variable <- submod
    df.mod$time <- row.names(df.mod)
  }
  
  ifelse(nchar(as.character(df.mod$time[1])) < 19,
         df.mod$time  <- as.POSIXct(x=df.mod$time, format = "%Y-%m-%d"),
         df.mod$time  <- as.POSIXct(x=df.mod$time, format = "%Y-%m-%d %H:%M:%S")
  )
  df.mod$location  <- factor(df.mod$location,levels = locmod)
  #df.mod$species   <- factor(df.mod$species, levels = submod)
  return(df.mod)
}

#' Get model results for different modelruns,
#'
#' Function to get modelresults for the same location and variables for multiple runs
#' 
#' @param filename the name of the file containg the binairy output
#' @param locs list of locations to be be extracted
#' @param vars list of variables to be be extracted
#' @return A dataframe with model output values for \code{submod} and \code{locmod}.
#' @examples
#' library(Waternet)
#' submod <- c("OXY", "Cl")
#' locmod <- c("LOX003","LOX009")
#' df <- get_model_data("DATA/testdata.his", locmod, submod)
#' library(ggplot2)
#' plot <- ggplot(df, aes(time, value)) +
#'   geom_line(aes(color = variable), size = 1) +
#'   facet_grid((variable ~ location), scales = "free")
#' plot
get_model_data <- function(filename, locs, vars) {
  data <- sobek2arr(filename)
  tim.attr <- attr(data, 'dimnames')[[1]] ## times
  loc.attr <- attr(data, 'dimnames')[[2]] ## locations
  var.attr <- attr(data, 'dimnames')[[3]] ## substances
  
  c.locs <- setdiff(locs, loc.attr)
  if (length(c.locs) != 0) {
    print(paste("Location is missing:",setdiff(locs, loc.attr)))
    print(paste("available: ", loc.attr))}
  
  c.subs <- setdiff(vars, var.attr)
  if (length(c.subs) != 0) {
    print(paste("Variable is missing:",setdiff(vars, var.attr)))
    print(paste("available: ", var.attr))}
  
  rundata <- arr2df(data,as.character(locs),vars) 
  
  rm(data)
  
  return(rundata)
}


#' Get model results for different modelruns,
#'
#' Function to get modelresults for the same location and variables for multiple runs
#' 
#' @param runs dataframe containing columns  <path> and <tag>"
#' @param filename the name of the file containg the binairy output
#' @param locs list of locations to be be extracted
#' @param vars list of variables to be be extracted
#' @return A dataframe with model output values for \code{submod} and \code{locmod}.
#' @examples
#' library(Waternet)
#' filename  <- c("DATA/testdata.his",
#'                "DATA/testdata2.his")
#' tag      <- c("run 1",
#'               "run 2")
#' runs <- data.frame(filename, tag)
#' submod <- c("OXY", "Cl")
#' locmod <- c("LOX003","LOX009")
#' df <- get_model_data("DATA/testdata.his", locmod, submod)
#' library(ggplot2)
#' plot <- ggplot(df, aes(time, value)) +
#'   geom_line(aes(color = variable), size = 1) +
#'   facet_grid((variable ~ location), scales = "free")
#' plot
get_runs_data <- function(runs, locs, vars) {
  # check on column names:
  `%not_in%` <- Negate(`%in%`)
  if ("filename" %not_in% names(runs)) {
    stop("runs dataframe is either missing the <filename> column")
  }
  if ("tag" %not_in% names(runs)) {
    stop("runs dataframe is either missing the <run> column")
  }  
  
  list.tmp <- list() 
  ilist <- 1
  #Model runs, loop, read results per run, add runidentifier as "tag":
  for (irun in 1:nrow(runs)){
    fn <- as.character(runs$filename[irun])
    tmp <- get_model_data(fn, locs, vars)
    
    # add tag to df:
    tmp$tag <- runs$tag[irun]
    list.tmp[[ilist]] <- tmp
    # Increase counter for resullts list
    ilist <- ilist + 1
    
  }
  rundata <- do.call("rbind",list.tmp) #combine all vectors into a matrix
  
  return(rundata)
}
