# data functions functions:


#' Create cumulative plots for dicharge series,
#'
#' Function to compute cumulative values, i,e. cumulative volumes based on flow values (m3/s) and the dt (s) in the dataset.
#' Cum values are resetted every year. 
#'
#' @param df the input dataframe, containing <time>, <location>, <variable>, <value> and <tag>
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
  require(dplyr)
  require(lubridate)
  
  .check_df_names(df,c("time", "variable", "location", "value", "tag") )
  df.tmp <- df %>%
    mutate(year = year(time)) %>%
    na.omit()
  if (debug) {print(head(df.tmp))}
  
  # Get dt:
  dt <- as.numeric(df.tmp$time[2]) - as.numeric(df.tmp$time[1])
  if (debug) {print(paste("timestep =", dt))}
  
  # check for column named <tag>
  if ("tag" %not_in% names(df.tmp)) {
    print("column <tag> added with  label model")
    df.tmp$tag <- "model"
  }
  
  # determine unique years, locations, vars and runid's:
  years <- unique(df.tmp$year)
  locs <- unique(df.tmp$location)
  vars <- unique(df.tmp$variable)
  tags <- unique(df.tmp$tag)
  
  # loop over unique years, locs, vars, tag to obtain yearly cum values.
  list.tmp <- list() 
  ilist <- 1
  for (y in years) {
    if (debug) {print(y)}
    for (loc in locs) {
      for (var in vars) {
        for (t in tags) {
          if (debug) {print(tag)}
          
          tmp <- df.tmp %>%
            filter(year == y,
                   location == loc,
                   variable == var,
                   tag == t) %>%
            arrange(time) %>%
            mutate(value.cum = cumsum(value*dt)) %>%
            select(-year)
          if (debug) {print(paste("number of records:",nrow(tmp)))}
          
          list.tmp[[ilist]] <- tmp
          # Increase counter for resullts list
          if (debug) {print(ilist)}
          if (debug) {print(unique(tmp$tag))}
          if (debug) {print(unique(tmp$variable))}
          if (debug) {print(unique(tmp$location))}
          ilist <- ilist + 1
        }
      }
    }
  }
  df.cumsum <- do.call("rbind",list.tmp) #combine all vectors into a matrix
  return(df.cumsum)
}


#' Compute average concentrations per (mass balance) area per meteotype
#'
#' Function to compute the average concentrations per (mass balance) area per meteotype
#' Meteotypes are periods of more or less equal hydrological conditions, ie dry summers, wet summer, etc.
#'
#' @param map.wqloc_swt mapping table with the relation between measurment location and (mass balance) area. 
#' @param df.wq the df containing WQ measurement data, containing <time>, <location>, <variable>, <value> values
#' @param df.period dataframe contating information regarding 
#' @param debug flag for debug output, default = FALSE
#' @param f.subs filter for substances, by default set to "all" to process all variables available in the <variable> column
#' @param f.meteotype filter for meteotypes, as integer 
#' @return An R dataframe containing cumnulative values, based on value * dt.
wqmeas_swt_meteotype <- function(df.map, df.wq, df.period, swt, f.meteotype = NULL) {
  require(dplyr)
  
  # Check header of WQ dataframe
  .check_df_names(df.wq,c("datum","meetpunt.id","meetwaarde","parameternaam") )
  
  
  if (is.numeric(f.meteotype)) {
    df.period <- df.period %>%
      filter(meteotype %in% f.meteotype)
  }
  # force to lower:
  names(df.wq) <- tolower(names(df.wq))
  df.wq <- df.wq %>%
    mutate(time = as.POSIXct(datum, format("%Y-%m-%d", tz = "CET")),
           month = month(time),
           year = year(time)) %>%
    rename(id = meetpunt.id,
           value = meetwaarde,
           variable = parameternaam) %>%
    select(id, variable, value, time, month, year)
  
  # Join with period
  df <- left_join(df.wq, df.period) %>%
    select(-delwaq_time,-real_time) %>%
    na.omit()
  
  # Join with location:
  df <- left_join(df, df.map) %>%
    select(-name,-x,-y) %>%
    na.omit()
  
  # Compute average per month, per swt
  df <- df %>%
    group_by(swt,variable,year, month,meteotype_label) %>%
    summarise(value = mean(value),
              n = n())
  return(df)
}
