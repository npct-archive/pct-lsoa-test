#Convert routes to a raster based on the number of cyclists
#WHen rasterizing it is necessary to not have overlapping lines
#So lines a broken into groups that do not overlap

#1) Create a table of lines and which group they are in
#2) Create a intraction matrix of lines that do and don't overlap
#3a) Outer Loop - Loop thought lines from least overlapping to most overlapping
#3b) Inner Loop - Having idetified a line in Outer loop, loop though all of its 
#non over lapping lines added to the group then checking that remaing don't overlap with each other


#libs
library(rgeos)
library(sp) 
library(rgdal)
library(dplyr)
library(utils)

#Inputs
lines_master = readRDS("../pct-bigdata/msoa/rf_nat.Rds")
lines_master@data = subset(lines_master@data, select=c("id"))
lines_master <- lines_master[!duplicated(lines_master$id),] #remove when rf_nat fixed
lines_data = readRDS("../pct-bigdata/msoa/l_nat.Rds")
lines_data@data = subset(lines_data@data, select=c("id","bicycle"))
lines_data <- lines_data[!duplicated(lines_data$id),] #remove when rf_lines fixed

size_limit = 2000

#join in the cycling data
lines_master@data = merge(lines_master@data,lines_data@data, by = "id")
#merge <- right_join(lines_master@data, lines_data@data, by = "id")
remove(lines_data)
#lines_master <- lines_master[1:100000,] #for low ram computers

#Simplify IDs
id2id <- data.frame(id_old=lines_master$id,id_new=1:nrow(lines_master))
lines_master$id <- id2id$id_new


#Check if too many lines to do at once
if(nrow(lines_master) < size_limit){
  goes = 1
} else {
  goes = ceiling(nrow(lines_master)/size_limit)
}

#Loop for when too many lines
for(v in 1:goes){
print(paste0("Doing loop ",as.character(v)," of ",as.character(goes)," at ",Sys.time()))
#lines <- lines_master[seq.int(from = v, to = nrow(lines_master), goes),] # Subset evenly across the country
lines = lines_master[(1 + (v-1)*size_limit):(v*size_limit),]
matrix_master= gIntersects(lines, byid = T) 
colnames(matrix_master) <- lines$id
rownames(matrix_master) <- lines$id
matrix = matrix_master
groups =  data.frame(id=as.integer(lines$id),nrow=as.integer(0)) #create a table of IDs to store which group they should go in
pb <- winProgressBar(title="Grouping progress bar", min=0, max=nrow(lines), initial=0, label="0 lines done")
progress = 0
#Outer Loop
for(i in 1:nrow(lines)){ #loop thought every line
  rowsum = data.frame(name=as.integer(rownames(matrix)),count=as.integer(rowSums(matrix)))
  if(nrow(matrix) == 0){ 
    break()
  }
  else {
    #max1 = max(rowsum$count)
    row1 = as.integer(rowsum$name[rowsum$count == max(rowsum$count)][1])
    #line1 = lines[lines$id == row1,]
    groups[groups$id==row1,2] <- i
    #groups[groups$id==line1$id,2] <- i
    partners1 = matrix[rownames(matrix) == row1,]
    partners_name1 = names(partners1[partners1 == FALSE])
    #matrix = matrix[!(rownames(matrix) %in% row1),!(colnames(matrix) %in% row1), drop = F]
    matrix = matrix[rownames(matrix) != row1,colnames(matrix) != row1, drop = F]
    submatrix = matrix[(rownames(matrix) %in% partners_name1),(colnames(matrix) %in% partners_name1), drop = F]
    subrowsum = data.frame(name=as.integer(rownames(submatrix)),count=as.integer(rowSums(submatrix)))
    #Inner Loop
    
    for(j in 1:nrow(submatrix)){
      if(nrow(submatrix) == 0){
        break()
      }
      else {
        
        #max2 = max(subrowsum$count)
        row2 = as.character(subrowsum$name[subrowsum$count == max(subrowsum$count)][1])
        #line2 = lines[lines$id == row2,]
        groups[groups$id==row2,2] <- i
        #groups[groups$id==line2$id,2] <- i
        partners2 = submatrix[rownames(submatrix) == row2,]
        test = subrowsum
        #test2 = partners2 * 1
        
        
        #########
        
        #Thrying to come up with a way not to do rowsum for everyloop as requires a very large number of calcualtions
        
        
        
        ###################
        
        subrowsum$count = subrowsum$count - partners2
        subrowsum = subrowsum[subrowsum$name != row2,]
        
        #partners2 = matrix[row2,]
        #matrix = matrix[!(rownames(matrix) %in% row2),!(colnames(matrix) %in% row2), drop = F]
        matrix = matrix[rownames(matrix) != row2,colnames(matrix) != row2, drop = F]
        #submatrix = submatrix[!(rownames(submatrix) %in% row2),!(colnames(submatrix) %in% row2), drop = F]
        submatrix = submatrix[rownames(submatrix) != row2,colnames(submatrix) != row2, drop = F] 
        partners_name2 = names(partners2[partners2 == FALSE])
        #submatrix = submatrix[(rownames(submatrix) %in% partners_name2),(colnames(submatrix) %in% partners_name2), drop = F]
        submatrix2 = submatrix[(rownames(submatrix) %in% partners_name2),, drop = F]
        submatrix_temp = submatrix2[,!(colnames(submatrix) %in% partners_name2) , drop = F]
        test$count = subrowsum$count - 
        progress = progress + 1
        info <- sprintf("%d lines done", progress)
        setWinProgressBar(pb, progress, label = info)
      }
    }
  }
  
}
close(pb)

#Rename IDs

foo <- left_join(groups, id2id, by = c("id" = "id_new"))

saveRDS(foo,paste0("../pct-lsoa-test/data/groups/groups_",v,".Rds"))



#Clean up some variaibles
remove(matrix)
remove(groups)
remove(matrix_master)
remove(lines)
remove(rowsum)

}

