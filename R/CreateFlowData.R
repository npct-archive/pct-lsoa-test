###
# THis file creates a set of LSOA - WZ data from OA - LZ data
#

#libs
require(rgdal)
library(dplyr)
library(stplanr)
library(maptools)
library(utils)

# Read in OA-WZ data
OA2WZ <- read.csv("E:/OneDrive - University of Leeds/Cycling Big Data/Flow/WU03BUK_oa_wz_v4/wu03buk_oa_wz_v4.csv", header = F)
nrow(OA2WZ)
names(OA2WZ) <- c("residence","workplace", "all", "home", "car", "other")
# Read in OA & WZ & LSOA centroids
cents_OA <- readOGR(dsn = "E:/OneDrive - University of Leeds/Cycling Big Data/Data/England_oa_2011_centroids", layer = "england_oa_2011_centroids")
cents_WZ <- readOGR(dsn = "E:/OneDrive - University of Leeds/Cycling Big Data/Data/England_wz_2011_centroids", layer = "england_wz_2011_centroids")
cents_LSOA <- readOGR(dsn = "E:/OneDrive - University of Leeds/Cycling Big Data/Data/England_lsoa_2011_centroids", layer = "england_lsoa_2011_centroids")
cents_LSOA@data$name <- NULL
#Reproject
cents_LSOA <- spTransform(cents_LSOA, CRS("+init=epsg:4326"))
cents_WZ <- spTransform(cents_WZ, CRS("+init=epsg:4326"))


#remove flows that don't start at an OA and end at a WZ
OA2WZ <- OA2WZ[OA2WZ$residence %in% cents_OA$code,]
OA2WZ <- OA2WZ[OA2WZ$workplace %in% cents_WZ$code,]

nrow(OA2WZ)
#match <- OA2WZ$residence %in% cents_WZ$code
#summary(match)

#Read in OA to LSOA lookup table
OA2LSOA <- read.csv("E:/OneDrive - University of Leeds/Cycling Big Data/Lookups/OA to LSOA/OA11_LSOA11.csv", header = T)
OA2LSOA <- OA2LSOA[,1:2]

#Join in the LSOA for each OA
OA2WZ <- left_join(OA2WZ,OA2LSOA, by = c("residence" = "OA11CD"))
nrow(OA2WZ)

OA2WZ[OA2WZ$LSOA11CD == "E01000001" & OA2WZ$workplace == "E33028869",]

#Group the LSOA together
OA2WZ <- OA2WZ[,2:7]
LSOA2WZ <- OA2WZ %>%
                    group_by(LSOA11CD,workplace) %>%
                    summarise_each(funs(sum))

LSOA2WZ <- as.data.frame(LSOA2WZ)

omatch = match(LSOA2WZ[[1]], cents_LSOA@data[[1]])
dmatch = match(LSOA2WZ[[2]], cents_WZ@data[[1]])
cents_o = cents_LSOA@coords[omatch,]
cents_d = cents_WZ@coords[dmatch,]
LSOA2WZ$dist <- geosphere::distHaversine(p1 = cents_o, p2 = cents_d)/1000
LSOA2WZ <- LSOA2WZ[LSOA2WZ$dist < 20,]
nrow(LSOA2WZ)

size_limit <- 50000

pb <- winProgressBar(title="Saving", min=0, max=ceiling(nrow(LSOA2WZ)/size_limit), initial=0, label="0 loops done")
progress = 0

for(i in 1:ceiling(nrow(LSOA2WZ)/size_limit)) {

  LSOA2WZ_sub <- LSOA2WZ[(1 + (i-1)*size_limit):(i*size_limit),]
  lines <- od2line(flow = LSOA2WZ_sub, zones = cents_LSOA, destinations = cents_WZ)
  saveRDS(lines,paste0("../pct-lsoa-test/data/lines_LSOA_WZ",i,".Rds"))
  progress = progress + 1
  info <- sprintf("%d loops done", progress)
  setWinProgressBar(pb, progress, label = info)
  
}


close(pb)

lines <- od2line(flow = LSOA2WZ, zones = cents_LSOA, destinations = cents_WZ)
saveRDS(lines,"../pct-lsoa-test/data/lines_LSOA_WZ.Rds")

match <- LSOA2WZ_test$workplace %in% cents_WZ$code

cents_LSOA$code <- as.character(cents_LSOA$code)
cents_WZ$code <- as.character(cents_WZ$code)

LSOA2WZ_test <- LSOA2WZ[1:100,]
LSOA2WZ_test$LSOA11CD <- as.character(LSOA2WZ_test$LSOA11CD)
LSOA2WZ_test$workplace <- as.character(LSOA2WZ_test$workplace)
lines <- od2line(zones = cents_LSOA, destinations = cents_WZ, flow = LSOA2WZ_test, zone_code = "code", dest_code = "code")


