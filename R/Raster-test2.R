## Malcolm Raster technique

library(gdalUtils)
src_dataset <- system.file("../pct-lsoa-test/data/rf.shp", package="gdalUtils")
gdal_rasterize(src_dataset,dst_filename = "../pct-lsoa-test/data/rf2.tif", a = "bike", tr = c(20,20))
