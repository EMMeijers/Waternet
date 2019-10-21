library(Waternet)
arr <- delwaq2arr(filename = "DATA/testdata.his")
dimnames(arr)
submod <- c("OXY", "Cl")
locmod <- c("LOX003","LOX009")
df <- arr2df(arr, locmod=locmod, submod=submod)
library(ggplot2)
plot <- ggplot(df, aes(time, value)) +
  geom_line(aes(color = variable), size = 1) +
  facet_grid((variable ~ location), scales = "free")
plot

get_data_locs(arr)
