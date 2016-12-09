## Malcolm Raster technique

library(gdalUtils)
library(rgdal)
library(raster)
gdal_setInstallation(search_path = "D:/Program Files/QGIS 2.18/bin", verbose = T)
src <- system.file("../pct/README_files/data/leeds-central-sample.shp", package="gdalUtils", mustWork = T)
src <- "D:\\Git\\pct-lsoa-test\\data\\line1.shp"
dst <- "D:\\Git\\pct-lsoa-test\\data\\test.tif"
#raster <- gdal_rasterize(src,dst, tr = c(500,500), verbose = T)
raster <- gdal_rasterize(src,dst, ts = c(1535,810), verbose = T)
raster <- gdal_rasterize(src,dst, verbose = T)
mask.raster <- raster(ncols = 100, nrows = 100)
raster <- rasterize(shape,mask.raster,field = "bike")
raster <- shp2raster(shape,mask.raster)

shape <- readOGR(dsn = "D:/Git/pct-lsoa-test/data", layer ="line1")

install.packages("rgdal")

#gdal_rasterize(src_dataset,dst_filename = "../pct-lsoa-test/data/rf2.tif", a = "bike", tr = c(20,20))
plot(raster)
library(graphics)


shp2raster <- function(shp, mask.raster, label, value, transform = FALSE, proj.from = NA,
                       proj.to = NA, map = TRUE) {
  require(raster, rgdal)
  
  # use transform==TRUE if the polygon is not in the same coordinate system as
  # the output raster, setting proj.from & proj.to to the appropriate
  # projections
  if (transform == TRUE) {
    proj4string(shp) <- proj.from
    shp <- spTransform(shp, proj.to)
  }
  
  # convert the shapefile to a raster based on a standardised background
  # raster
  r <- rasterize(shp, mask.raster)
  # set the cells associated with the shapfile to the specified value
  r[!is.na(r)] <- value
  # merge the new raster with the mask raster and export to the working
  # directory as a tif file
  r <- mask(merge(r, mask.raster), mask.raster, filename = label, format = "GTiff",
            overwrite = T)
  
  # plot map of new raster
  if (map == TRUE) {
    plot(r, main = label, axes = F, box = F)
  }
  
  names(r) <- label
  return(r)
}
