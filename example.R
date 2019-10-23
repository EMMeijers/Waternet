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
