# Helper function for Rotated Markers in leaflet
.chk.rotatedMarkers.files <- function(fn.rotatedMarkers) {
  for (fn in fn.rotatedMarkers) {
    if (!file.exists(fn)) {
      print(paste(fn, ": does not excist in", getwd()))
      # check all library paths:
      found <- F
      for (lib.path in .libPaths()) {
        fn.from <- paste0(lib.path,"/Waternet/data/", fn)
        if (file.exists(fn.from)) {
          file.copy(from=fn.from, to=fn)
          found <- T
          break()
        }
      }
      if (!found) {
        stop(paste("file could not be copied:", fn))
      }
      print(paste("file copied to workdir:", fn))
    }
  }
}


#' Add a leaflet layer containing rotated arrows:
#'
#' @param map leaflet map as base layer
#' @param data dataframe containg the folowing columns: 
#' 1) value [numeric] 
#' 2) angle in degrees (N = 0, clockwise) [numeric] 
#' 3) name of the location [character]
#' 4) lat of the location [numeric] 
#' 5) lon of the location  [numeric] 
#' @param unit of the value's plotted on the map
#' @param grouplater as layer option for showing/hiding the map layer
#' @param bins bins for the colormap used
#' @param plot.angle (default TRUE) of plotting direction of the value
#' @return leaflet map object.
RotatedMarker.layer <- function(map, data, unit,grouplayer, bins, plot.angle = T) {
  # function needs two files in the R workdir to be able to plot 
  # First, check if depended files are available:
  .chk.rotatedMarkers.files(c("Leaflet.rotatedMarker.js", "uparrow.svg"))
  
  unit <- gsub("m3", "m<sup>3</sup>", unit)
  
  # Set columnnames to lower case:
  colnames(data) <- tolower(colnames(data))
  .check_df_names(runs, c("value", "angle", "lat", "lon", "name"))
  
  labels <- sprintf(
    "<strong>%s</strong><br>%s %s",
    data$name, data$value, unit) %>% lapply(htmltools::HTML)
  
  pal <- colorBin("Spectral", domain = data$value, bins = bins, reverse = T)
  
  # make north arrow icon
  .north.arrow.icon <-
    makeIcon(iconUrl = "uparrow.svg",iconWidth = 15,iconHeight = 15)
  
  # add rotated marker
  .rotatedMarker <- 
    htmlDependency( name = "Leaflet.rotatedMarker" # note: this .js file has to be copied and stored in your current working directory 
                    , version = "0.1.2"
                    , src = normalizePath( path = getwd() )
                    , script = "leaflet.rotatedMarker.js")
  
  # this is taken from: https://gist.github.com/jcheng5/c084a59717f18e947a17955007dc5f92
  .registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  
  
  # Add RotatedMarkers:
  tmp.map <- map %>%
    registerPlugin( plugin = rotatedMarker ) %>%
    addCircleMarkers(data = data,
                     lng = ~lon,
                     lat = ~lat,
                     radius = 10, 
                     fillColor = ~pal(value), 
                     fillOpacity = 1,
                     group = grouplayer, 
                     stroke = FALSE)
  if (plot.angle) {
    tmp.map <- tmp.map %>%
      addMarkers(data = data,
                 lng = ~lon,
                 lat = ~lat,
                 icon = north.arrow.icon,
                 options = markerOptions(rotationAngle = ~angle), 
                 group = grouplayer, 
                 label = labels)
  }
  
  tmp.map <- tmp.map %>%
    addLabelOnlyMarkers(data = data,
                        lng = ~lon,
                        lat = ~lat,
                        label = labels,
                        group = "labels",
                        labelOptions = labelOptions(noHide = T, 
                                                    textOnly = F, 
                                                    offset = c(10,10), 
                                                    direction = "auto")) %>%
    addLayersControl(
      overlayGroups = grouplayer,
      options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup("labels")
  
  return(tmp.map)
}

