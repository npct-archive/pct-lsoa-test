#Convert routes to a raster based on the number of cyclists
#WHen rasterizing it is necessary to not have overlapping lines
#So lines a broken into groups that do not overlap

#1) Create a table of lines and which group they are in
#2) Create a intraction matrix of lines that do and don't overlap
#3a) Outer Loop - Loop thought lines from least overlapping to most overlapping
#3b) Inner Loop - Having idetified a line in Outer loop, loop though all of its 
#non over lapping lines added to the group then checking that remaing don't overlap with each other
#4) Loop though each group convert to raster then add up rasters

#227 groups using min min method 10,000 lines
#201 goups uisng max min methond 5,000 lines
#228 goups uisng min min methond 5,000 lines
#205 goups uisng max max methond 5,000 lines

#libs
library(rgeos)
library(raster)
library(sp)
library(rgdal)
library(dplyr)

#library(data.table)
#library(dplyr)
#library(gdalUtils)
#library(stplanr)

#Inputs
lines_master = readRDS("../pct-bigdata/rf_nat.Rds")
lines_master@data = subset(lines_master@data, select=c("id"))
lines_data = readRDS("../pct-bigdata/l_nat.Rds")
lines_data@data = subset(lines_data@data, select=c("id","bicycle"))

#join in the cycling data
lines_master@data = merge(lines_master@data,lines_data@data, by = "id")
remove(lines_data)

if(nrow(lines_master) < 10000){
  goes = 1
  #lines_master = lines_master_big
  #remove(lines_master_big)
}
else {
  goes = round(nrow(lines_master_big)/10000,0)
}

#subsets
#lines_master = sample(lines_master, size = 100)
#lines_master = lines_master[1:10000,]

for(v in 1:goes){
print(paste0("Doing loop ",as.character(v)," of ",as.character(goes)))

#read in an make files
lines = lines_master
matrix_master= gIntersects(lines, byid = T) # find overlapping lines #16s for 1

rownames(matrix_master) <- lines$id
colnames(matrix_master) <- lines$id
#matrix_master = 1 * matrix_master# convert T/F to 1/0
matrix = matrix_master # create duplicate for working on
groups =  data.frame(id=as.character(lines$id),nrow=as.integer(0)) #create a table of IDs to store which group they should go in

#igraph approach
#library(igraph)
#matrix_adj = !matrix_master
#graph = graph_from_adjacency_matrix(matrix_adj, mode = "undirected")
#plot(graph)
#cliques = cliques(graph, min = 5, max = 100)




#Outer Loop
for(i in 1:nrow(lines)){ #loop thought every line
  rowsum = data.frame(name=rownames(matrix),count=rowSums(matrix)) #Count the overlap for each line
  if(nrow(matrix) == 0){ #check if there are any lines in the matrix, no lines means we have finished
    print("no lines in the matrix")
    break()
  }
  else {
    max1 = max(rowsum$count)
    row1 = as.character(rowsum$name[rowsum$count == max1][1])
    line1 = lines[lines$id == row1,]
    groups[groups$id==line1$id,2] <- i
    #groups[which(groups[,1] == line1$id),2] <- i # Add this line to a group
    #print(paste0(line1$id," started new group ",i, " at ",as.character(Sys.time())))
    #partners1 = subset(matrix, rownames(matrix) == row1) #subset to just the relavant line
    partners1 = matrix[row1,]
    matrix = matrix[!(rownames(matrix) %in% row1),!(colnames(matrix) %in% row1), drop = F] #remove the alocated linn from the matrix #about 2 sec for 10k
    #We now have created a group with a line in it
    # Now find the lines that might also go into that group by creating a submatrix
    #sel1 = partners1[,FALSE] #remove lines that overlap
    #sel1 = !partners1[,1] #remove lines that overlap
    #partners_name1 = rownames(partners1)[sel1]
    partners_name1 = names(partners1[partners1 == FALSE])
    submatrix = matrix[(rownames(matrix) %in% partners_name1),(colnames(matrix) %in% partners_name1), drop = F]
    #Inner Loop
    #we now loop though the submatrix and add everything possible to the group
    for(j in 1:nrow(submatrix)){
      if(nrow(submatrix) == 0){ #check if there are any lines in the submatrix, no lines means we have finished
        print(paste0("Group ",i, " is complete with ",nrow(groups[groups$nrow==i,])," members. At ", as.character(Sys.time())))
        break()
      }
      else {
        subrowsum = data.frame(name=rownames(submatrix),count=rowSums(submatrix))
        #row2 = as.character(subset(subrowsum, count == min(subrowsum$count))[1,1]) # find the least overlapping line
        max2 = max(subrowsum$count)
        row2 = as.character(subrowsum$name[subrowsum$count == max2][1])
        #row2 = as.character(subset(subrowsum, count == max(subrowsum$count))[1,1]) # find the least overlapping line
        #line2 = lines[which(lines$id==row2),]
        line2 = lines[lines$id == row2,]
        groups[groups$id==line2$id,2] <- i
        #groups[which(groups[,1] == line2$id),2] <- i # Add this line to a group
        #print(paste0(line2$id," added to group ",i, " in the inner loop at ",as.character(Sys.time())))
        partners2 = matrix[row2,]
        #partners2 = subset(matrix, rownames(matrix) == row2) #subset to jsut the relavant line
        matrix = matrix[!(rownames(matrix) %in% row2),!(colnames(matrix) %in% row2), drop = F] #remove the alocated linn from the matrix
        submatrix = submatrix[!(rownames(submatrix) %in% row2),!(colnames(submatrix) %in% row2), drop = F] #remove the alocated linn from the matrix
        #sel2 = !partners2[1,] #remove lines that overlap
        #partners_name2 = colnames(partners2)[sel2]
        partners_name2 = names(partners2[partners2 == FALSE])
        submatrix = submatrix[(rownames(submatrix) %in% partners_name2),(colnames(submatrix) %in% partners_name2), drop = F]
       
      }
    }
  }
}

break()
 
#join in the cycling data
#lines_master@data$bicycle <- NULL
#lines_master@data[,"bicycle"] <- as.integer()
#lines_master@data$bicycle <- lines_data@data$bicycle[which(lines_master$id %in% lines_data$id)]

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
  #lines2raster@data = lines2raster@data[,-(1:5), drop=FALSE]
  lines2raster <- spTransform(lines2raster,CRS("+init=epsg:4267"))
  raster_sub <- rasterize(lines2raster,raster_master , field ="bicycle")
  raster_sub[is.na(raster_sub[])] <- 0 
  raster_stack <- overlay(raster_stack, raster_sub, fun=function(x,y){return(x+y)})
  raster_stack[is.na(raster_stack[])] <- 0
  print(paste0("added group ",as.character(k)," to the stack at ",as.character(Sys.time())))
}

#Replace the 0 with NA 
raster_fin <- raster_stack
raster_fin[which(raster_fin[] == 0)] <- NA
plot(raster_fin)

#Need to convert to 8bit

#Save results
writeRaster(raster_fin,filename = "../pct-lsoa-test/data/Nat4.tif", format ="GTiff")
write.csv(matrix_master, file = "../pct-lsoa-test/data/matrix.csv")
}