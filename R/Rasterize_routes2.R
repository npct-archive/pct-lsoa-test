#Convert routes to a raster based on the number of cyclists
#WHen rasterizing it is necessary to not have overlapping lines
#So lines a broken into groups that do not overlap

#1) Create a table of lines and which group they are in
#2) Create a intraction matrix of lines that do and don't overlap
#3a) Outer Loop - Loop thought lines from least overlapping to most overlapping
#3b) Inner Loop - Having idetified a line in Outer loop, loop though all of its 
#non over lapping lines added to the group then checking that remaing don't overlap with each other
#4) Loop though each group convert to raster then add up rasters

#libs
library(rgeos)
library(raster)
library(sp) 
library(rgdal)
library(dplyr)
library(utils)
library(velox)

#Inputs
lines_master = readRDS("../pct-bigdata/msoa/rf_nat.Rds")
lines_master@data = subset(lines_master@data, select=c("id"))
lines_master <- lines_master[!duplicated(lines_master$id),] #remove when rf_nat fixed
lines_data = readRDS("../pct-bigdata/msoa/l_nat.Rds")
lines_data@data = subset(lines_data@data, select=c("id","bicycle"))
lines_data <- lines_data[!duplicated(lines_data$id),] #remove when rf_lines fixed

size_limit = 5000

#join in the cycling data
lines_master@data = merge(lines_master@data,lines_data@data, by = "id")
#merge <- right_join(lines_master@data, lines_data@data, by = "id")
remove(lines_data)
lines_master <- lines_master[1:10000,] #for low ram computers

#Simplify IDs
id2id <- data.frame(id_old=lines_master$id,id_new=1:nrow(lines_master))
lines_master$id <- id2id$id_new
#remove(id2id)

#Set up the king raster
Xres <- as.integer(geosphere::distHaversine(c(lines_master@bbox[1,1],lines_master@bbox[2,1]), c(lines_master@bbox[1,2],lines_master@bbox[2,1]))/20)
Yres <- as.integer(geosphere::distHaversine(c(lines_master@bbox[1,1],lines_master@bbox[2,1]), c(lines_master@bbox[1,1],lines_master@bbox[2,2]))/20)
raster_king <- raster(ncols=Xres, nrows=Yres, ext = extent(lines_master), crs= "+init=epsg:4267", vals = 0)

#Check if too many lines to do at once
if(nrow(lines_master) < size_limit){
  goes = 1
} else {
  goes = ceiling(nrow(lines_master)/size_limit)
}

#Loop for when too many lines
for(v in 1:goes){
print(paste0("Doing loop ",as.character(v)," of ",as.character(goes)," at ",Sys.time()))
lines <- lines_master[seq.int(from = v, to = nrow(lines_master), goes),] # Subset evenly across the country
#lines = lines_master[(1 + (v-1)*size_limit):(v*size_limit),]
matrix_master= gIntersects(lines, byid = T) 
colnames(matrix_master) <- lines$id
rownames(matrix_master) <- lines$id
matrix = matrix_master
groups =  data.frame(id=as.character(lines$id),nrow=as.integer(0)) #create a table of IDs to store which group they should go in
pb <- winProgressBar(title="Grouping progress bar", min=0, max=nrow(lines), initial=0, label="0 lines done")
progress = 0
#Outer Loop
for(i in 1:nrow(lines)){ #loop thought every line
  rowsum = data.frame(name=rownames(matrix),count=rowSums(matrix)) 
  if(nrow(matrix) == 0){ 
    break()
  }
  else {
    max1 = max(rowsum$count)
    row1 = as.character(rowsum$name[rowsum$count == max1][1])
    line1 = lines[lines$id == row1,]
    groups[groups$id==line1$id,2] <- i
    partners1 = matrix[row1,]
    partners_name1 = names(partners1[partners1 == FALSE])
    matrix = matrix[!(rownames(matrix) %in% row1),!(colnames(matrix) %in% row1), drop = F]
    submatrix = matrix[(rownames(matrix) %in% partners_name1),(colnames(matrix) %in% partners_name1), drop = F]
    #Inner Loop
    
    for(j in 1:nrow(submatrix)){
      if(nrow(submatrix) == 0){
        break()
      }
      else {
        subrowsum = data.frame(name=rownames(submatrix),count=rowSums(submatrix))
        max2 = max(subrowsum$count)
        row2 = as.character(subrowsum$name[subrowsum$count == max2][1])
        line2 = lines[lines$id == row2,]
        groups[groups$id==line2$id,2] <- i
        partners2 = matrix[row2,]
        matrix = matrix[!(rownames(matrix) %in% row2),!(colnames(matrix) %in% row2), drop = F] 
        submatrix = submatrix[!(rownames(submatrix) %in% row2),!(colnames(submatrix) %in% row2), drop = F] 
        partners_name2 = names(partners2[partners2 == FALSE])
        submatrix = submatrix[(rownames(submatrix) %in% partners_name2),(colnames(submatrix) %in% partners_name2), drop = F]
        progress = progress + 1
        info <- sprintf("%d lines done", progress)
        setWinProgressBar(pb, progress, label = info)
      }
    }
  }
}
close(pb)

foo <- left_join(groups, id2id, by.x = "id", by.y = "id_old")

break()

#Set up the raster
raster_master <- crop(raster_king,extent(lines))
raster_stack<- raster_master

#loop though each group and rasterize
#then add the raster onto the last raster
print(paste0("Grouping complete for loop ",v," starting raster process for ",as.character(max(groups[,2]))," groups at ",Sys.time()))
pb <- winProgressBar(title="Raster progress bar", min=0, max=max(groups[,2]), initial=0, label="0 rasters done")
progress = 0
for(k in 1:max(groups[,2]) ){  #max(groups[,2])
  checklist = groups[which(groups[,2] == k),]
  lines2raster = lines[which(lines$id %in% checklist[,1]),]
  lines2raster <- spTransform(lines2raster,CRS("+init=epsg:4267"))
  raster_sub <- rasterize(lines2raster,raster_master , field ="bicycle", filename= "data/temp/singletemp.grd", overwrite = TRUE)
  raster_sub[is.na(raster_sub[])] <- 0 
  raster_stack <- overlay(raster_stack, raster_sub, fun=function(x,y){return(x+y)}, filename = "data/temp/overlaytemp.grd", overwrite = TRUE)
  raster_stack[is.na(raster_stack[])] <- 0
  progress = progress + 1
  info <- sprintf("%d rasters done", progress)
  setWinProgressBar(pb, progress, label = info)
  #print(paste0("added group ",as.character(k)," to the stack at ",as.character(Sys.time())))
}
close(pb)

#Replace the 0 with NA 
raster_stack[which(raster_stack[] == 0)] <- NA

#Need to convert to 8bit

#Save results
writeRaster(raster_stack,filename = paste0("../pct-lsoa-test/data/Raster/Raster",v,".tif"), format ="GTiff")
print(paste0("Rasetering complete for loop ",v," at ",Sys.time()))

file.remove("data/temp/singletemp.grd")
file.remove("data/temp/overlaytemp.grd")

#Clean up some variaibles
remove(matrix)
remove(groups)
remove(matrix_master)
remove(lines)
remove(rowsum)
}

