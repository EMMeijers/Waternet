# Mass balance functions:
#' Aggregate loads per Mass balance area, term, substance and meteo type
#'
#' Function to compute the load in a mass balance area per meteotype
#' Meteotypes are periods of more or less equal hydrological conditions, ie dry summers, wet summer, etc.
#'
#' @param df.bal the mass balance input dataframe, containing <stof>, <tag>, <month>, <year>, <location>, <meteotype>, <meteotype_label>,
#' <term>, <richting>, <surface_m2>
#' @param f.subs filter for substances, by default set to "all" to process all variables available in the <variable> column
#' @param f.meteotype filter for meteotypes, as integer 
#' @return An R dataframe containing loads per balance term
loads_in_swt_meteotype <- function(df.bal, f.subs ="all", f.meteotype = NULL) {
  require(dplyr)
  
  .check_df_names(df.bal,c("stof","tag","month","year","location","meteotype","meteotype_label","term", "richting", "surface_m2") )
  df.tmp <- df %>%
    mutate(year = year(time)) %>%
    na.omit()
  if (debug) {print(head(df.tmp))}
  
  
  # filters:
  if (f.subs != "all") {
    df.bal <- df.bal %>%
      filter(stof %in% f.subs)
  }
  if (is.numeric(f.meteotype)) {
    df.bal <- df.bal %>%
      filter(meteotype %in% f.meteotype)
  }
  
  df.load <- df.bal %>%
    filter(richting == "In",
           value != 0) %>%
    group_by(stof,tag,month,year,location, meteotype_label,term, richting, surface_m2) %>%
    summarise(g_dag = sum(value)/(365/12)) %>%
    group_by(stof,meteotype_label,location,tag,term, richting, surface_m2) %>%
    summarise(g_dag = mean(g_dag)) %>%
    mutate(load = round(g_dag * 1000 /surface_m2,2),
           eenheid = "mg/dag m2") %>%
    select(-g_dag) %>%
    spread(tag, load) %>%
    ungroup()
  
  return(df.load)
}

#' Aggregate loads per Mass balance area, term, substance and month
#'
#' Function to compute the load in a mass balance area on a monthly interval
#'
#' @param df.bal the mass balance input dataframe, containing <stof>, <tag>, <month>, <year>, <location>,
#' <term>, <richting>, <surface_m2>
#' @param f.subs filter for substances, by default set to "all" to process all variables available in the <variable> column
#' @return An R dataframe containing loads per balance term
loads_in_swt_month <- function(df.bal, f.subs ="all") {
  require(dplyr)
  
  .check_df_names(df.bal,c("stof","tag","month","year","location","term", "richting", "surface_m2") )
  df.tmp <- df %>%
    mutate(year = year(time)) %>%
    na.omit()
  if (debug) {print(head(df.tmp))}
  
  
  # filters:
  if (f.subs != "all") {
    df.bal <- df.bal %>%
      filter(stof %in% f.subs)
  }
  
  
  df.load <- df.bal %>%
    filter(richting == "In",
           value != 0) %>%
    group_by(stof,month,year, meteotype_label,location, tag,term, richting, surface_m2) %>%
    summarise(load = sum(value)/(365/12)) %>%
    mutate(eenheid = "g/dag") %>%
    ungroup()
  
  return(df.load)
}

#' Compute HRT per Mass balance area per meteotype
#'
#' Function to compute the HRT in a mass balance area per meteotype
#' Meteotypes are periods of more or less equal hydrological conditions, ie dry summers, wet summer, etc.
#'
#' @param df.bal the mass balance input dataframe, containing <stof>, <tag>, <month>, <year>, <location>,<meteotype>, <meteotype_label>,
#' <richting>, <volume_m3>
#' @param f.subs filter for substances, by default set to "all" to process all variables available in the <variable> column
#' @return An R dataframe HRT per Mass balance area
hrt_swt_meteotype <- function(df.bal, f.meteotype = NULL){
  require(dplyr)
  
  .check_df_names(df.bal,c("stof","tag", "month","year", "meteotype", "meteotype_label","location", "richting", "volume_m3") )
  df.tmp <- df %>%
    mutate(year = year(time)) %>%
    na.omit()
  if (debug) {print(head(df.tmp))}
  
  #filter on meteotypes:
  if (is.numeric(f.meteotype)) {
    df.bal <- df.bal %>%
      filter(meteotype %in% f.meteotype)
  }
  df.HRT <- df.bal %>%
    filter(stof == "Continuity",
           richting != "-") %>%
    group_by(stof,meteotype,meteotype_label,month,year,location, richting, volume_m3, tag) %>%
    summarise(value = sum(value) / (365/12)) %>%
    group_by(stof,meteotype,meteotype_label,location, richting,volume_m3, tag) %>%
    summarise(m3_dag = mean(value)) %>%
    spread(richting, m3_dag) %>%
    mutate(HRT = round(volume_m3/-Out,2)) %>%
    ungroup()
  
  df.HRT.tabel <- df.HRT %>%
    select(-In, -Out) %>%
    mutate(eenheid = "dagen") %>%
    spread(meteotype, HRT)
  
  return(df.HRT.tabel)
}


#' Compute HRT per Mass balance area per month
#'
#' Function to compute the HRT in a mass balance area per month
#'
#' @param df.bal the mass balance input dataframe, containing <stof>, <tag>, <month>, <year>, <location>
#' <richting>, <volume_m3>
#' @param f.subs filter for substances, by default set to "all" to process all variables available in the <variable> column
#' @return An R dataframe HRT per Mass balance area
hrt_swt_month <- function(df.bal, f.meteotype = NULL){
  require(dplyr)
  
  .check_df_names(df.bal,c("stof","tag", "month","year","location", "richting", "volume_m3") )
  df.tmp <- df %>%
    mutate(year = year(time)) %>%
    na.omit()
  if (debug) {print(head(df.tmp))}
  
  
  df.HRT <- df.bal %>%
    filter(stof == "Continuity",
           richting != "-") %>%
    group_by(stof,month,year,location, richting, volume_m3, tag) %>%
    summarise(m3_dag = sum(value) / (365/12)) %>%
    spread(richting, m3_dag) %>%
    mutate(HRT = round(volume_m3/-Out,2)) %>%
    ungroup()
  
  
  return(df.HRT)
}

