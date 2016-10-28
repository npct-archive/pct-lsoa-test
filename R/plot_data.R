#####
## For Quick plotting and comparison
####

# Libs
library(leaflet)
library(sp)
library(rgeos)


# Get data
lsoa <- readRDS("data/rf_LSOA_Cam.Rds")
oa <- readRDS("data/rf_OA_Cam.Rds")
msoa <- readRDS("../pct-data/cambridgeshire/rf.Rds")
lsoa_simp <- gSimplify(lsoa, 0.5, topologyPreserve = TRUE)
oa_simp <- gSimplify(oa, 0.5, topologyPreserve = TRUE)
gIsValid(lsoa_simp)
gIsSimple(lsoa_simp)


names(lsoa@data)
nrow(lsoa)
nrow(msoa)

#plot the results

leaflet() %>% 
  setView(0.12, 52.205, 14) %>% 
  addProviderTiles("Stamen.Toner") %>% 
  addPolylines(data = oa, color ="green", weight = 1) %>%
  addPolylines(data = lsoa_simp, color ="red", weight = 1) %>%
  addPolylines(data = msoa, color ="blue", weight = 1)

