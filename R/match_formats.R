# Compare the demo LSoA data to some MSOA data
#get demo data
c_LSOA <- readRDS("../pct-data/lsoa_demo/c.Rds")
l_LSOA <- readRDS("../pct-data/lsoa_demo/l.Rds")
rf_LSOA <- readRDS("../pct-data/lsoa_demo/rf.Rds")
rq_LSOA <- readRDS("../pct-data/lsoa_demo/rq.Rds")
rnet_LSOA <- readRDS("../pct-data/lsoa_demo/rnet.Rds")
z_LSOA <- readRDS("../pct-data/lsoa_demo/z.Rds")

#get MSOA data
c_MSOA <- readRDS("../pct-data/cambridgeshire/c.Rds")
l_MSOA <- readRDS("../pct-data/cambridgeshire/l.Rds")
rf_MSOA <- readRDS("../pct-data/cambridgeshire/rf.Rds")
rq_MSOA <- readRDS("../pct-data/cambridgeshire/rq.Rds")
rnet_MSOA <- readRDS("../pct-data/cambridgeshire/rnet.Rds")
z_MSOA <- readRDS("../pct-data/cambridgeshire/z.Rds")

#Create Duplicates to work on

c  <- c_LSOA
l  <- l_LSOA
rf  <- rf_LSOA
rq  <- rq_LSOA
rnet  <- rnet_LSOA
z <- z_LSOA

#add columns to LSOA data
#C
c_names <- names(c_MSOA)
c@data[,c_names] <- NA
c@data$geo_code <- c@data$code
c@data$geo_label <- c@data$name
c@data$code <- NULL
c@data$name <- NULL


l <- l_LSOA[,c(1,2,3,44,65,79,93,107,128,149,170,191,212,233,255)]
names(l@data) <- c("msoa1","msoa2","all")
l_names <- names(l_msoa[,c(1:70)])

names(l)
names(l_MSOA)


