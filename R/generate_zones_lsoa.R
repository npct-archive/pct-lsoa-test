#creat zones file for LSOA test

library(rgdal)

zones_all <- readOGR("D:/Users/earmmor/OneDrive - University of Leeds/Cycling Big Data/Data/England_lsoa_2011_gen_clipped","england_lsoa_2011_gen_clipped")
#cents = geojsonio::geojson_read("../pct-lsoa-test/data/lsoa_centroids.geojson", what = "sp")
#cents = cents[grep(pattern = "Camb", x = cents$name),]
zones = zones_all[grep(pattern = "Camb", x = zones_all$name),]
#nrow(cents)
#zones <- zones_all$code %in% cents$code
#zmatch <- match(zones_all$code, cents$code)
nrow(zones)
plot(zones)
zones <- spTransform(zones, CRS(proj4string(cents))) # transform CRS
#plot(over(zones,cents))

#library(leaflet)

#leaflet() %>% 
#  setView(0.12, 52.205, 14) %>% 
#  addProviderTiles("Stamen.Toner") %>% 
#  addPolylines(data = zones, color ="green", weight = 1) %>%
#  addMarkers(data = cents)

#plot(zones)

saveRDS(zones,file = "../pct-lsoa-test/data/zones_LSOA_Cam.Rds")
