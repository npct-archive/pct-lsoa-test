#Velox Test
library(velox)

# Creat Velox raster object

## From matrix
mat <- matrix(1:100, 10, 10)
vx <- velox(mat, extent=c(0,1,0,1), res=c(0.1,0.1), crs="+proj=longlat +init=epsg:3857")

## Rasterize polygons using "id" column
vx$rasterize(spdf, field="id", band=1)

