
# aim find LSOA that best matches WZ

library(dplyr)

# load in lookups
OA2LSOA <- read.csv("E:/OneDrive - University of Leeds/Cycling Big Data/Lookups/OA to LSOA/OA11_LSOA11.csv", header = T)
OA2LSOA <- OA2LSOA[,1:2]
OA2WZ <- read.csv("E:/OneDrive - University of Leeds/Cycling Big Data/Lookups/OA to WZ/OA11_WZ11.csv", header = T)
OA2WZ <- OA2WZ[,1:2]
OAWZLSOA <- left_join(OA2WZ, OA2LSOA, by = "OA11CD")

OAWZLSOA$WZ11CD <- as.character(OAWZLSOA$WZ11CD)
OAWZLSOA$LSOA11CD <- as.character(OAWZLSOA$LSOA11CD)
WZLSOA <- OAWZLSOA[,2:3]
WZLSOA <- within(WZLSOA, { nWZ <- ave(WZ11CD, LSOA11CD, FUN=function(x) length(unique(x)))})
WZLSOA <- within(WZLSOA, { nLSOA <- ave(LSOA11CD, WZ11CD, FUN=function(x) length(unique(x)))})

WZLSOA$nWZmax <- NULL
WZLSOA$nLSOAmax <- NULL

for(x in 1:nrow(WZLSOA)){
  WZLSOA$nWZmax[x] <- max(WZLSOA$nWZ[WZLSOA$WZ11CD == WZLSOA$WZ11CD[x]])
  WZLSOA$nLSOAmax[x] <- max(WZLSOA$nLSOA[WZLSOA$LSOA11CD == WZLSOA$LSOA11CD[x]])
}

WZLSOA$class <- NULL

for(x in 1:nrow(WZLSOA)){
  if(WZLSOA$nWZmax[x] == 1 & WZLSOA$nLSOAmax[x] == 1) {
    WZLSOA$class[x] <- "Exact Match" 
  }
  else if(WZLSOA$nWZmax[x] > 1 & WZLSOA$nLSOAmax[x] == 1){
    WZLSOA$class[x] <- "LSOA made up of WZ" 
  }
  else if(WZLSOA$nWZmax[x] == 1 & WZLSOA$nLSOAmax[x] > 1){
    WZLSOA$class[x] <- "WZ made up of LSOA" 
  } 
  else {
    WZLSOA$class[x] <- "Complex" 
  } 
}
WZLSOA$class <- as.factor(WZLSOA$class)
saveRDS(WZLSOA, "../pct-lsoa-test/data/WZLSOA.Rds")

WZLSOA_unique <- unique(WZLSOA[,c(1,7)])
saveRDS(WZLSOA_unique, "../pct-lsoa-test/data/WZLSOA_unique.Rds")
