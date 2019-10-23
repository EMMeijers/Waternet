# data functions functions:


#' Create cumulative plots for dicharge series,
#'
#' Function to compute cumulative values, i,e. cumulative volumes based on flow values (m3/s) and the dt (s) in the dataset 
#'
#' @param df the input dataframe, containing <time>, <location>, <variable>, <value> values
#' @param debug flag for debug output, default = FALSE
#' @return An R dataframe containing cumnulative values, based on value * dt.
#' @examples
#' library(Waternet)
#' submod <- c("OXY")
#' locmod <- c("LOX010")
#' df <- get_model_data("DATA/testdata.his", locmod, submod)
#' df.cum <- cum_values(df, debug=T)
#' library(ggplot2)
#' plot <- ggplot(df.cum, aes(time, value.cum)) +
#'   geom_line(aes(color = variable), size = 1) +
#'   facet_grid((variable ~ location), scales = "free")
#' plot
cum_values <- function(df, debug = F) {
  library(dplyr)
  library(lubridate)
  
  .check_df_names(df,c("time", "variable", "location", "value") )
  print("here")
  df.tmp <- df %>%
    mutate(year = year(time)) %>%
    na.omit()
  
  # Get dt:
  dt <- as.numeric(df.tmp$time[2]) - as.numeric(df.tmp$time[1])
  
  # check for column named <tag>
  # `%not_in%` <- Negate(`%in%`)
  if ("tag" %not_in% names(df.tmp)) {
    print("column <tag> added with value \"model\"")
    df.tmp$tag <- "model"
  }
  
  # determine unique years, locations, vars and runid's:
  years <- unique(df.tmp$year)
  locs <- unique(df.tmp$location)
  vars <- unique(df.tmp$variable)
  tags <- unique(df.tmp$tag)
  
  # loop over unique years, locs, vars, runs to obtain yearly cum values.
  list.tmp <- list() 
  ilist <- 1
  for (y in years) {
    if (debug) {print(y)}
    for (loc in locs) {
      for (var in vars) {
        for (tag in tags) {
          tmp <- df.tmp %>%
            filter(year == y,
                   location == loc,
                   variable == var,
                   tag == tag) %>%
            arrange(time) %>%
            mutate(value.cum = cumsum(value*dt))
          
          list.tmp[[ilist]] <- tmp
          # Increase counter for resullts list
          ilist <- ilist + 1
        }
      }
    }
  }
  df.cumsum <- do.call("rbind",list.tmp) #combine all vectors into a matrix
  return(df.cumsum)
}
