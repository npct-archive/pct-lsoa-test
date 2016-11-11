#Convert routes to a raster based on the number of cyclists
#WHen rasterizing it is necessary to not have overlapping lines
#So lines a broken into groups that do not overlap

#1) Create a table of lines and which group they are in
#2) Create a intraction matrix of lines that do and don't overlap
#3a) Outer Loop - Loop thought lines from least overlapping to most overlapping
#3b) Inner Loop - Having idetified a line in Outer loop, loop though all of its 
#non over lapping lines added to the group then checking that remaing don't overlap with each other
#4) Loop though each group convert to raster then add up rasters

#Inputs
lines_master = readRDS("../pct-data/isle-of-wight/rf.Rds")
lines_data = readRDS("../pct-data/isle-of-wight/l.Rds")


#libs
library(rgeos)
library(raster)
library(sp)
library(rgdal)
library(gdalUtils)
library(stplanr)


#read in an make files
lines = lines_master
matrix_master= gIntersects(lines, byid = T) # find overlapping lines
rownames(matrix_master) <- lines$id
colnames(matrix_master) <- lines$id
matrix_master = 1 * matrix_master# convert T/F to 1/0
matrix = matrix_master # create duplicate for working on
groups =  data.frame(id=as.character(lines$id),nrow=as.integer(0)) #create a table of IDs to store which group they should go in

#Outer Loop
for(i in 1:nrow(lines)){ #loop thought every line
  rowsum = data.frame(name=rownames(matrix),count=rowSums(matrix)) #Cound the overlap for each line
  if(nrow(matrix) == 0){ #check if there are any lines in the matrix, no lines means we have finished
    print("no lines in the matrix")
    break()
  }
  else {
    #print("some lines in the matrix")
    row1 = as.character(subset(rowsum, count == min(rowsum$count))[1,1]) # find the least overlapping line
    line1 = lines[which(lines$id==row1),]
    groups[which(groups[,1] == line1$id),2] <- i # Add this line to a group
    print(paste0(line1$id," added to group ",i, " in the outer loop"))
    partners1 = subset(matrix, rownames(matrix) == row1) #subset to jsut the relavant line
    matrix = matrix[!(rownames(matrix) %in% row1),!(colnames(matrix) %in% row1), drop = F] #remove the alocated linn from the matrix
    #We now have created a group with a line in it
    # Now find the lines that might also go into that group by creating a submatrix
    sel1 = !partners1[1,] #remove lines that overlap
    partners_name1 = colnames(partners1)[sel1]
    submatrix = matrix[(rownames(matrix) %in% partners_name1),(colnames(matrix) %in% partners_name1), drop = F]
    #Inner Loop
    #we now loop though the submatrix and add everything possible to the group
    for(j in 1:nrow(submatrix)){
      if(nrow(submatrix) == 0){ #check if there are any lines in the submatrix, no lines means we have finished
        print("no lines in the sub matrix")
        break()
      }
      else {
        subrowsum = data.frame(name=rownames(submatrix),count=rowSums(submatrix))
        row2 = as.character(subset(subrowsum, count == min(subrowsum$count))[1,1]) # find the least overlapping line
        line2 = lines[which(lines$id==row2),]
        groups[which(groups[,1] == line2$id),2] <- i # Add this line to a group
        print(paste0(line2$id," added to group ",i, " in the inner loop"))
        partners2 = subset(matrix, rownames(matrix) == row2) #subset to jsut the relavant line
        matrix = matrix[!(rownames(matrix) %in% row2),!(colnames(matrix) %in% row2), drop = F] #remove the alocated linn from the matrix
        submatrix = submatrix[!(rownames(submatrix) %in% row2),!(colnames(submatrix) %in% row2), drop = F] #remove the alocated linn from the matrix
        sel2 = !partners2[1,] #remove lines that overlap
        partners_name2 = colnames(partners2)[sel2]
        submatrix = submatrix[(rownames(submatrix) %in% partners_name2),(colnames(submatrix) %in% partners_name2), drop = F]
       
      }
    }
  }
}
 
#join in the cycling data
lines_master@data$bike <- NULL
lines_master@data[,"bike"] <- as.integer()
lines_master@data$bike <- lines_data@data$bicycle

#Set up the raster
Xres <- as.integer(geosphere::distHaversine(c(lines_master@bbox[1,1],lines_master@bbox[2,1]), c(lines_master@bbox[1,2],lines_master@bbox[2,1]))/20)
Yres <- as.integer(geosphere::distHaversine(c(lines_master@bbox[1,1],lines_master@bbox[2,1]), c(lines_master@bbox[1,1],lines_master@bbox[2,2]))/20)
raster_master <- raster(ncols=Xres, nrows=Yres, ext = extent(lines_master), crs= "+init=epsg:4267", vals = 0)
raster_stack<- raster_master

#loop though each group and rasterize
#then add the raster onto the last raster
for(k in 1:max(groups[,2]) ){  #max(groups[,2])
  checklist = groups[which(groups[,2] == k),]
  lines2raster = lines_master[which(lines_master$id %in% checklist[,1]),]
  lines2raster@data = lines2raster@data[,-(1:5), drop=FALSE]
  lines2raster <- spTransform(lines2raster,CRS("+init=epsg:4267"))
  raster_sub <- rasterize(lines2raster,raster_master , field ="bike")
  raster_sub[is.na(raster_sub[])] <- 0 
  raster_stack <- overlay(raster_stack, raster_sub, fun=function(x,y){return(x+y)})
  raster_stack[is.na(raster_stack[])] <- 0
}

#Replace the 0 with NA 
raster_fin <- raster_stack
raster_fin[which(raster_fin[] == 0)] <- NA
plot(raster_fin)

#Need to convert to 8bit

#Save results
writeRaster(raster_fin,filename = "../pct-lsoa-test/data/IoW.tif", format ="GTiff")

