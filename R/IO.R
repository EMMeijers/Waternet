# IO functions:


#' convert Delwaq <.map> or <.his> file into an R array object,
#'
#' @param filename the <.his> or <.map> file to be converted.
#' @return An R array object of the Delwaq <.his> or <.map> file named  \code{filename}.
#' @examples
#' library(Waternet)
#' arr <- his2arr(filename = "DATA/NZBLOOM.his", timestamp = F, begintime = "2003-01-01 00:00:00")
#' dimnames(arr)
#' submod <- c("Chlfa", "OXY")
#' locmod <- c("NZR6NW020", "NZR9TS010")
#' df <- arr2df(arr, locmod=locmod, submod=submod)
#' df$value[df$variable == "fResptot"] <- -df$value[df$variable == "fResptot"]
#' library(ggplot2)
#' plot <- ggplot(df, aes(time, value))
#' plot +
#'   geom_line(aes(color = variable), size = 1) +
#'   geom_point(aes(color = variable), fill = "white",  shape = 21, size = 4) +
#'   facet_grid((. ~ location))
delwaq2arr <- function (filename, timestamp = T, begintime = "1900-01-01 00:00:00"){
  
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
  vars <- attr(data, 'dimnames')[[1]] ## locations
  return(vars)
}

#' Get times from data array,
#'
#' @param 'data object (array).
#' @return list of times in data
get_data_tims <- function(data) {
  tims <- attr(data, 'dimnames')[[3]] ## locations
  return(tims)
}
