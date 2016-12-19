rf = readRDS("../pct-data/west-yorkshire/rf.Rds")
l = readRDS("../pct-data/west-yorkshire/l.Rds")

# install sfr branch of stplanr and sf
devtools::install_github(repo = "robinlovelace/stplanr", ref = "sfr-dep")
devtools::install_github("edzer/sfr")
library(sf)
sf::st_line_sample
system.time(
  {p = stplanr::line_sample(l = rf, n = 1e5, weights = l$bicycle)}
)
plot(p)
library(dplyr)
library(leaflet.extras)
leaflet() %>%
  addTiles() %>%
  addWebGLHeatmap(lng = p@coords[,1], lat = p@coords[,2], size = 10, units = "px", alphaRange = 0.0001)
