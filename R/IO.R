# IO functions:


#' convert SOBEK <.map> or <.his> file into an R array object,
#'
#' @param filename the <.his> or <.map> file to be converted.
#' @return An R array object of the SOBEK <.his> or <.map> file named  \code{filename}.
#' @examples
#' library(Waternet)
#' arr <- sobek2arr(filename = "data/testdata.his")
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
  if (typeof(filename) != "character") {filename <- as.character(filename)}
  
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


#' extract data from array into a dataframe for selected locations and substances,
#'
#' @param arr the array to be extracted.
#' @param locmod the locations in the array to be extracted
#' @param submod the substances in the array to be extracted
#' @return A dataframe with model output values for \code{submod} and \code{locmod}.
#' @examples
#' library(Waternet)
#' arr <- sobek2arr(filename = "data/testdata.his")
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
#' df <- get_model_data("data/testdata.his", locmod, submod)
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
#' filename  <- c("data/testdata.his",
#'                "data/testdata2.his")
#' tag      <- c("run 1",
#'               "run 2")
#' runs <- data.frame(filename, tag)
#' submod <- c("OXY", "Cl")
#' locmod <- c("LOX003","LOX009")
#' df <- get_model_data("data/testdata.his", locmod, submod)
#' library(ggplot2)
#' plot <- ggplot(df, aes(time, value)) +
#'   geom_line(aes(color = variable), size = 1) +
#'   facet_grid((variable ~ location), scales = "free")
#' plot
get_runs_data <- function(runs, locs, vars) {
  # check on column names:
  `%not_in%` <- Negate(`%in%`)
  chk <- c("filename", "tag")
  .check_df_names(runs, chk)

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

#' get SOBEK <.map> or <.his> file location names,
#'
#' @param filename the <.his> or <.map> file.
#' @return A vector object of the location names.
#' @examples
#' library(Waternet)
#' vars <- get_his_vars("DATA/testdata.his")
get_his_locs <- function(filename) {
  `%not_in%` <- Negate(`%in%`)
  if (typeof(filename) != "character") {filename <- as.character(filename)}
  
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
    duname <- seq(1:afm[2])
  }
  duname <- str_trim(duname)
  close(zz)
  return(duname)
}

#' get SOBEK <.map> or <.his> file variable names,
#'
#' @param filename the <.his> or <.map> file.
#' @return A vector object of the variable names.
#' @examples
#' library(Waternet)
#' vars <- get_his_vars("DATA/testdata.his")
get_his_vars <- function(filename) {
  `%not_in%` <- Negate(`%in%`)
  if (typeof(filename) != "character") {filename <- as.character(filename)}
  
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
  
  scu.sym  <- readChar(zz, 1)
  readChar(zz, 1)
  afm <- readBin(zz, integer(), n = 2)
  
  syname <- vector("character", afm[1])
  for (i in 1:afm[1]) {
    syname[i] <- readChar(zz, 20)
  }

  syname <- str_trim(syname)
  close(zz)
  return(syname)
}

# function OpenMapFile
# opens the file, reads the header and some constants (including the names of the substances)
# returns a list containing all specifications of the map file
# this list is later used to directly approach the data in the file
# This function does not read any data itself, but prepares for the function ReadMapFile

#' opens the map file, reads the header and some constants
#'
#' @param filename the filename of the map file to be read.
#' @return A dataframe with header and some constants of \code{filename}.
#' @examples
OpenHISFile<-function(filename){
  require("stringr")
  # prepare the list of file characteristics. Most are NA at this moment, only file type
  # and file name are known
  MFS<-list(FileType="map",FileName=filename,ByteOrder=NA,FormatType=NA,Header=NA,T0=NA,
            TUnit=NA,TStep=NA,NumSegm=NA,SegmName=NA,NumSubs=NA,SubsName=NA,NBytesBlock=NA,
            DataStart=NA,NTimes=NA)
  ## Open file in binary mode
  zz <- file(filename, "rb")
  ## Read header lines
  h1<-readChar(zz,40)
  h2<-readChar(zz,40)
  h3<-readChar(zz,40)
  # store the header (120 bytes character string) in MFS
  MFS$Header<-paste(h1,h2,h3,sep='')
  # empty lines:
  readChar(zz, 4)
  MFS$T0 <- readChar(zz, 19)

  # empty lines:
  readChar(zz, 7)
  scu.prep <- readChar(zz, 8)
  scu  <- as.numeric(scu.prep) # check for the internal timer
  sign.scu <- sign(scu)  # sign can be +1, 0 or -1
  
  ifelse(is.na(sign.scu), dec.sign.scu <- NA,   # NB. no error handling here yet, NA should only occur if timestamp = F and then you don't need dec.sign.scu
         ifelse(sign.scu == 1 , dec.sign.scu <- "*",
                ifelse(sign.scu == -1, dec.sign.scu <- "/",
                       ifelse(sign.scu == 0, stop("The sign of your internal timer is neither negative nor positive, but 0."), stop("Check dec.sign.scu.")))))
  MFS$TStep <- scu
  MFS$TUnit<-readChar(zz,1)
  ## Read 2 integers (number of substances and number of segments) and store in MFS
  MFS$NumSubs<-readBin(zz,integer(),n=1)
  MFS$NumSegm<-readBin(zz,integer(),n=1)
  ## reserve some memory for storing the names of the substances
  MFS$SubsName <- vector("character",MFS$NumSubs)
  ## Now a row of characters (length 20 bytes) with the names of the substances
  for(i in 1:MFS$NumSubs){
    MFS$SubsName[i] <- readChar(zz,20)
  }
  # Determine the position in the file where data start: after header, time information and
  # names of substances
  MFS$DataStart = (MFS$NumSubs*20)+(40*4)+(4*2)
  # data end at the end of the file, which is obtained with a generic function
  MFS$DataEnd   = file.info(filename)$size
  # one block of data (= one moment in time), has length given by space for time information
  # (4 bytes) + NumSegm*NumSubs data of 4 bytes each
  MFS$NBytesBlock = (MFS$NumSegm*MFS$NumSubs+1)*4
  # number of times stored in the file is equal to length of data divided by length of one
  # time block
  MFS$NTimes = (MFS$DataEnd-MFS$DataStart)/MFS$NBytesBlock
  # now the file is (technically) closed again, just to be sure. It will be reopened at each
  # read operation. This ensures that there do not remain random open files in the program
  close(zz)
  # the function returns the list with file characteristics, that can be used in subsequent
  # read operations
  return(MFS)
}

# function ReadMapFile is used to read a number of substances from a predetermined number
# of segments. It does so by first reading an entire block corresponding to one time slice,
# storing these data in a segment*substance matrix, and finally selecting only the segments
# and substances required
# Subs and Segms are vectors containing the sequential numbers of the substances and segments
# that one wants to read. Default is all segments and all substances. Default time is the
# first time block

#' opens the map file, reads map output data for a number of substances for one time slice
#'
#' @param MFS A dataframe with information on the map file, returned from OpenMapFile\(\).
#' @return A matrix with model map output values for \code{MFS}.
#' @examples
ReadHISFile<-function(MFS,Subs=seq(1,MFS$NumSubs),HSegms=seq(1,MFS$NumSegm/MFS$NumLay),VSegms=seq(1,MFS$NumLay),Time=1){
  require("stringr")
  BotLay=(VSegms==c(MFS$NumLay))
  # re-open the file, based on the file name
  zz<-file(MFS$FileName,"rb")
  # position at the start of the time block required
  if (BotLay)offLay<-(MFS$NBytesBlock-4)/MFS$NumLay*(MFS$NumLay-1)+4 else offLay<-4
  seek(zz,where=MFS$DataStart+(Time-1)*MFS$NBytesBlock)
  # read time information (not really used for now, but could be useful later, and anyway
  # this is required to start reading the data from the right position)
  tim<- readBin(zz,integer(),n=1,size=4)
  seek(zz,where=MFS$DataStart+(Time-1)*MFS$NBytesBlock+offLay)
  # read the data block corresponding to the time asked
  BlockLen<-MFS$NumSegm*MFS$NumSubs
  if(BotLay)BlockLen<-BlockLen/MFS$NumLay
  datblock<-readBin(zz,"double",n=BlockLen,size=4)
  # close the file
  close(zz)
  # rearrange the data read into a matrix of segments (rows) by substances (columns)
  # note that the data in the file are arranged sequentially by blocks of all substances
  # for a segment, then all substances for next segments etc. Therefore the matrix is
  # stored by row (byrow=T)
  dts<-matrix(data=datblock,nrow=BlockLen/MFS$NumSubs,ncol=MFS$NumSubs,byrow=T)
  # replace -999 by NA
  dts[dts< -900]<-NA
  # only keep the segments and substances required, rest is thrown away
  if (BotLay)Segms=HSegms else{
    Segms<-vector(length=0)
    for (i in 1:length(Vsegms))Sgems=c(Segms,HSegms*VSegms[i])
  }
  dts<-dts[Segms,Subs]
  # return the matrix with required substances and segments
  return(dts)
}


  