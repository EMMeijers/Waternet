### Example Get results from HIS file, convert to df, make plot:

library(devtools)
install_github("EMMeijers/Waternet")

library(Waternet)
arr <- sobek2arr(filename = "DATA/testdata.his")
submod <- c("OXY", "Cl")
locmod <- c("LOX003","LOX009")
df <- arr2df(arr, locmod=locmod, submod=submod)
library(ggplot2)
plot <- ggplot(df, aes(time, value)) +
  geom_line(aes(color = variable), size = 1) +
  facet_grid((variable ~ location), scales = "free")
plot


### Get model results, make plot:
library(Waternet)
df <- get_model_data("DATA/testdata.his", locmod, submod)
library(ggplot2)
plot <- ggplot(df, aes(time, value)) +
  geom_line(aes(color = variable), size = 1) +
  facet_grid((variable ~ location), scales = "free")
plot


### Get run data, make plot:

library(Waternet)
# create df with columns <filename> and <tag>: 
filename  <- c("DATA/testdata.his",
               "DATA/testdata2.his")
tag      <- c("run 1",
              "run 2")
runs <- data.frame(filename, tag)

submod <- c("OXY", "Cl")
locmod <- c("LOX003","LOX009")
df <- get_runs_data(runs, locmod, submod)
library(ggplot2)
plot <- ggplot(df, aes(time, value)) +
  geom_line(aes(color = tag), size = 1) +
  facet_grid((variable ~ location), scales = "free")
plot

### Check for incorrect column names:

fn  <- c("DATA/testdata.his",
               "DATA/testdata2.his")
tag2      <- c("run 1",
              "run 2")
runs2 <- data.frame(fn, tag2)
df <- get_runs_data(runs2, locmod, submod)


### Get data, make cum value plot:
library(Waternet)
submod <- c("OXY")
locmod <- c("LOX010")
df <- get_model_data("DATA/testdata.his", locmod, submod)
                     
df.cum <- cum_values(df, debug=F)
library(ggplot2)
plot <- ggplot(df.cum, aes(time, value.cum)) +
  geom_line(aes(color = variable), size = 1) +
  facet_grid((variable ~ location), scales = "free")
plot

### Add RotatedMarker to leaflet map:
library(Waternet)
library(leaflet)
library(htmltools)
library(htmlwidgets)
# create df with columns <filename> and <tag>: 
lat  <- c(52, 53)
lon      <- c(4.89,5.1)
name <- c("pointing North", "Pointing East")
angle <- c(0,90)
value <- c(1,2)
df <- data.frame(name, angle, value, lat,lon)

bins <- c(0,1,2,Inf)
# Add leaflet map:
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTopoMap)

m <- RotatedMarker.layer(m, df, "m3/uur", "pietje", bins)
m

library(Waternet)
locs <- get_his_locs("DATA/testdata.his")
vars <- get_his_vars("DATA/testdata.his")

library(Waternet)
tmp <- OpenHISFile("DATA/testdata.his")
tmp$Header
tmp$T0
tmp$TUnit
tmp$TStep


df <- ReadHISFile()