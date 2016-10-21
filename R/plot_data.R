#####
## For Quick plotting and comparison
####

# Libs
library(leaflet)
library(sp)


# Get data
lsoa <- readRDS("data/rf_LSOA_Cam.Rds")
msoa <- readRDS("../pct-data/cambridgeshire/rf.Rds")


#plot the results

leaflet() %>% 
  setView(0.07, 52, 12) %>% 
  addPolylines(data = lsoa, color ="red", weight = 1) %>%
  addPolylines(data = msoa, color ="blue", weight = 1)

