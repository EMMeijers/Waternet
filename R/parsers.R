#' read DELWAQ monitor.dat inlcude file into an R dataframe object,
#' the file contains the monitoring area's as aplied in a Sobek model. 
#' 
#' @param filename monitoring file to be read.
#' @return R dataframe containing the folowing columns: .
parse_DELWAQ_monitor_file <- function(filename) {
  require(stringr)
  # Read file, cleanup whitespaces and quotes
  data <- readLines(filename)
  data <- gsub("'","",data) 
  data <- gsub(' +',' ',data) 
  data <- trimws(data)
  list.tmp <- list() 
  ilist <- 1
  first_SWT_found <- F
  noseg_found <- F
  
  for (i in 1:length(data)) {
    # Read line by line, split on whitespace":
    tmp.string <- str_split(data[i], " ")
    for (tmp_text in tmp.string[[1]]) {
      # Find SWT statement:
      # Find noseg integer:
      if (grepl("SWT_", tmp_text)) {
        monarea <- tmp_text
        # reset noseg flag:
        first_SWT_found <- T
        noseg_found <- F
        next
      }
      if (!noseg_found & first_SWT_found) {
        if(suppressWarnings(!is.na(as.integer(tmp_text)))) {
          noseg <- as.integer(tmp_text)
          noseg_found <- T
        }
        next
      }
      
      if (noseg_found & first_SWT_found & nchar(tmp_text > 0)) {
        segid <- as.integer(tmp_text)
      }
      if (first_SWT_found) {
        #start writing dataframe
        tmp.df <- data.frame("monarea" = monarea, "noseg" = noseg, "segments" = segid)
        list.tmp[[ilist]] <- tmp.df
        ilist <- ilist + 1
      }
    }
  }
  
  df <- do.call("rbind",list.tmp) #combine all vectors into a matrix
  return(df)
}

#' read DELWAQ ntrdlwq.poi file into an R dataframe object,
#' the file contains the relation between the Netter flow objects and the DELWAQ segments. 
#' 
#' @param filename ntrdlwq file to be read.
#' @return R dataframe containing the folowing columns: 1) segment,2) reach .
parse_ntrdlwq_file <- function(filename) {
  require(stringr)
  # Read file, cleanup whitespaces and quotes
  # Read file, cleanup whitespaces and quotes
  data <- readLines(filename)
  data <- gsub("\"","",data) 
  data <- gsub(' +',' ',data) 
  data <- trimws(data)
  
  # first loop to get keyword sections:
  
  list.tmp <- list() 
  ilist <- 1
  # Delwaq Netter mapping:
  for (i in 1:length(data)) {
    # Check for strings containing "#"
    if (grepl("# ", data[i])) {
      tmp.df <- data.frame("keyword" = data[i], "linenumber" = i)
      list.tmp[[ilist]] <- tmp.df
      ilist <- ilist + 1
    }
  }
  keys <- do.call("rbind",list.tmp) #combine all vectors into a matrix
  start <- keys$linenumber[1]
  stop <- keys$linenumber[2]
  
  data.sel <- data[start:stop]
  rm(keys, data)
  
  # Second part
  list.tmp <- list() 
  ilist <- 1
  for (i in 1:length(data.sel)) {
    # Check for strings containing "#"
    if (grepl("Segment ", data.sel[i])) {
      tmp.string <- str_split(data.sel[i], " ")
      segid <- as.integer(tmp.string[[1]][2])
      reaches <- str_split(data.sel[i+1], ",")
      tmp.df <- data.frame("segment" = segid,
                           "type" = "reach",
                           "id" = as.character(reaches[[1]][2:length(reaches[[1]])]))
      list.tmp[[ilist]] <- tmp.df
      ilist <- ilist + 1
      nodes <-  str_split(data.sel[i+2], ",")
      if (length(nodes[[1]]) > 1) {
        tmp.df <- data.frame("segment" = segid,
                             "type" = "node",
                             "id" = as.character(nodes[[1]][2:length(nodes[[1]])]))
        list.tmp[[ilist]] <- tmp.df
        ilist <- ilist + 1  
      }
    }
  }
  segments <- do.call("rbind",list.tmp) #combine all vectors into a matrix
  rm(data.sel)
  
  return(segments)
}

#' read DELWAQ pointer.dat file into an R dataframe object,
#' the file the relation between the delwaq segments. 
#' 
#' @param filename DELWAQ pointer file to be read.
#' @return R dataframe containing the folowing columns: 1) from segment number,2) to segment number.
parse_DELWAQ_pointer_file <- function(filename) {
  require(stringr)
  data <- readLines(filename)
  data <- gsub(' +',' ',data) 
  data <- trimws(data)
  
  data.split <- str_split(data, " ")
  tmp <- data.frame(matrix(unlist(data.split), nrow=length(data.split), byrow=T))
  colnames(tmp) <- c("from", "to", "from-1", "to-1")
  
  # select from and to, convert to integers:
  tmp <- tmp[1:2]
  tmp$from <- as.integer(as.character(tmp$from))
  tmp$to <- as.integer(as.character(tmp$to))
  
  return(tmp)
}
