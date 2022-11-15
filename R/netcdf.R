#' Function to get absolute times from nc file. 
#' 
#' @param nc netcdf file.
#' @return dataframe containing the absolute times from \code{nc}
#' @example 
#' 
getNcAbsTimes <- function(nc) {
  
  # Get unit
  t.unit <- nc$dim$time$units
  
  # Check for time unit: "seconds since"
  if (!str_detect(t.unit, "seconds since")) {
    stop("Time unit is not in seconds")
  } 
  # Get T0
  t0 <- as.POSIXct(str_remove(t.unit, "seconds since "))
  
  #Get relative time:
  times <- data.frame(reltime_s = ncvar_get(nc, "time")) %>%
    mutate(datetime = reltime_s + t0)
  
  return(times)
}

#' Get model results for different modelruns,
#'
#' Function to get modelresults for the same location and variables for multiple runs
#' 
#' @param nc netcdf file.
#' @param var variable to be be extracted
#' @param obj list of variables to be be extracted
#' @return A dataframe with model output values for \code{var} and \code{obj} for each time step.
#' @examples
#' library(Waternet)
#' df <- getNcTimeSeries(nc.his, "discharge_magnitude","station_id")
#' library(ggplot2)
#' plot <- ggplot(df.o, aes(x = datetime, y = value, col = location)) + 
#'   geom_line()
#' plot
getNcTimeSeries <- function(nc, var, obj) {
  #Get times:
  times <- getNcAbsTimes(nc)
  
  #Get object id's
  obj.names <- str_trim(ncvar_get(nc, obj), side = "right")
  
  if (length(obj.names) == 1) {
    df.tmp <- data.frame(ncvar_get(nc, var))} 
  else {
    df.tmp <- data.frame(t(ncvar_get(nc, var)))}
  
  names(df.tmp) <- obj.names
  
  df <- cbind(times, df.tmp) %>% 
    select(-reltime_s) %>%
    pivot_longer(!datetime, names_to = "location", values_to = "value") %>%
    mutate(variable = var) %>%
    select(datetime, location, variable, value) %>%
    arrange(location, variable, datetime)
  
  return(df)
}

