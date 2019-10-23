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

### Get vars, locations 
get_data_locs(arr)
get_data_vars(arr)


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