####
# Make Raster tiles
####

##Libs
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(RColorBrewer)

### load data
lines <- readRDS("data/rf_LSOA_Cam2.Rds")
lines <- spTransform(lines, CRS("+init=epsg:21037"))
plot(lines)

raster <- raster()
extent(raster) <- extent(lines)
res(raster) <- 2500

raster2 <- rasterize(lines,raster)















#lines <- CRS("+proj=longlat +datum=WGS84")
raster <- raster(extent(lines), crs = projection(lines), nrow=5, ncol=5)
raster <- c(1:25)
plot(raster)

lengths <- for(i in 1:25){
  #i <- 3
  #tmp_rst <- raster[i]
  #tmp_rst[i] <- 1
  tmp_shp <- rasterToPolygons(raster[i])
  lines_crp <- crop(lines, tmp_shp)
  raster[i] <- gLength(lines_crp)/1000
  remove(tmp_shp)
  remove(tmp_rst)
  remove(lines_crp)
  #plot(raster)
  print(paste0("finished cell ",as.character(i)))
}


spplot(raster, scales = list(draw = TRUE), xlab = "x", ylab = "y", 
       col.regions = colorRampPalette(brewer.pal(9, "YlOrRd")), 
       sp.layout = list("sp.lines", lines), 
       par.settings = list(fontsize = list(text = 15)), at = seq(0, 1800, 200))

plot(raster)




lengths <- sapply(1:ncell(raster), function(i) {
  tmp_rst <- raster
  tmp_rst[i] <- 1
  tmp_shp <- rasterToPolygons(tmp_rst)
  
  if (gIntersects(lines, tmp_shp)) {
    lines_crp <- crop(lines, tmp_shp)
    lines_crp_length <- gLength(lines_crp)
    raster[i] <- lines_crp_length/1000
    #return(lines_crp_length/1000)
  } else {
    raster[i] <- 0
    #return(0)
  }
  print(paste0("finished cell ",as.character(i)))
})