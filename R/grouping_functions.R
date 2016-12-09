#Functions for gropuing routes

library(rgeos)
library(sp)
library(dplyr)
library(utils)
library(stplanr)
library(dplyr)
library(maptools)

grouplines_bearing <- function(lines_master, group_no, chunk_size){
  #Groups lines quickly by will not find a group for all lines
  #Check if too many lines to do at once
  if(nrow(lines_master) < chunk_size){
    goes = 1
  } else {
    goes = ceiling(nrow(lines_master)/chunk_size)
  }
  #Set up df to store results
  groups_master = data.frame(id_old = as.character(lines_master$id), id_new = as.integer(1:nrow(lines_master)),bearing=as.integer(NA),group=as.integer(NA))
  rowsum_master = data.frame(id_new = as.integer(),count=as.integer())
  #Rename IDs so that there are list of intergers
  lines_master$id = groups_master$id_new

  # Stage 1: find the bearing for all the lines
  #Do bearings in chunks becuase fails over 10,000
  bearing_res <- data.frame(id_new = as.integer(), bearing = as.integer())
  for(v in 1:goes){
    if((v*chunk_size)>=nrow(lines_master)){l_max = nrow(lines_master)}else{l_max = (v*chunk_size)}
    lines = lines_master[(1 + (v-1)*chunk_size):l_max,]
    bearing <- round(line_bearing(lines),0)
    for(h in 1:length(bearing)){
      if(bearing[h]<=0){bearing[h]=bearing[h]+180}
      if(bearing[h]==0){bearing[h]=180} # first if intorduces some 0 so remove them
    }
    bearing <- data.frame(id_new = as.integer(lines$id), bearing = as.integer(bearing))
    bearing_res <- rbind(bearing_res,bearing)
  }
  groups_master$bearing <- bearing_res$bearing[match(groups_master$id_new, bearing_res$id_new)]
  print(paste0("Bearings found for all lines at ",Sys.time()))
  remove(bearing, bearing_res, lines, goes, v, h, l_max)

  #Stage 2: For each bearing creat an interaction matrix and find the easy groups
  for(i in 1:180){
    id_list <- groups_master$id_new[groups_master$bearing == i]
    lines = lines_master[lines_master$id %in% id_list,]
    matrix_master = gIntersects(lines, byid = T)
    rownames(matrix_master) = lines$id
    colnames(matrix_master) = lines$id
    rowsum = data.frame(id_new = rownames(matrix_master),count=as.integer(rowSums(matrix_master)))
    rowsum_master = rbind(rowsum_master,rowsum)
    twos = as.character(rowsum$id_new[rowsum$count <= 2])
    matrix = matrix_master[(rownames(matrix_master) %in% twos),(colnames(matrix_master) %in% twos), drop = F]
    rowsum2 = data.frame(id_new = rownames(matrix),count=as.integer(rowSums(matrix)))
    ones = as.character(rowsum2$id_new[rowsum2$count == 1])
    groups_master$group[groups_master$id_new %in% ones] <- group_no
    group_no = group_no + 1

  }
  print(paste0("Of ",nrow(groups_master)," lines, ",nrow(groups_master[!is.na(groups_master$group),])," have been assigend a group on the fist attempt, at ",Sys.time()))
  return(groups_master)
}

############################################################################################

grouplines_grid <- function(lines_master, group_no, grid_size){
  #Set up df to store results
  groups_master = data.frame(id_old = as.character(lines_master$id), id_new = as.integer(1:nrow(lines_master)),group=as.integer(NA))
  #rowsum_master = data.frame(id_new = as.integer(),count=as.integer())
  #Rename IDs so that there are list of intergers
  lines_master$id = groups_master$id_new
  lines_master <- spTransform(lines_master, CRS("+proj=utm +zone=30 +datum=WGS84"))
  points = SpatialLinesMidPoints(lines_master)

  ### define SpatialGrid object
  bb <- bbox(points)
  cs <- c(grid_size, grid_size)
  cc <- bb[, 1] + (cs/2)  # cell offset
  cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd <- SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)), proj4string=CRS(proj4string(points)))

  #Assign Grid IDs to Lines
  groups_master$grid <- as.integer(NA)
  over <- over(points, sp_grd)
  groups_master$grid <- over$id
  groups = groups_master
  #groups_master = groups_master[with(groups_master,order(-count)),]
  #over_summary = data.frame(table(groups_master$grid))
  remove("over","bb","cs","cc","cd","grd")
  ones = 1

  #Loop Though Creating Groups - not working keeps overrighting it self
  for(k in 1:100000){
    if(length(ones) == 0){break()}
    set = groups[!duplicated(groups$grid), ]
    lines = lines_master[lines_master$id %in% set$id_new,]
    matrix_master = gIntersects(lines, byid = T)
    rownames(matrix_master) = lines$id
    colnames(matrix_master) = lines$id
    rowsum = data.frame(id_new = rownames(matrix_master),count=as.integer(rowSums(matrix_master)))
    #nrow(rowsum[rowsum$count == 1,])
    lows = as.character(rowsum$id_new[rowsum$count <= 2])
    matrix = matrix_master[(rownames(matrix_master) %in% lows),(colnames(matrix_master) %in% lows), drop = F]
    rowsum2 = data.frame(id_new = rownames(matrix),count=as.integer(rowSums(matrix)))
    #nrow(rowsum2[rowsum2$count == 1,])
    ones = as.character(rowsum2$id_new[rowsum2$count == 1])
    groups_master$group[groups_master$id_new %in% ones] <- group_no
    group_no = group_no + 1
    groups = groups[!(groups$id_new %in% ones),]
    #lines_ug = lines_ug[!(lines_ug$id %in% ones),]
  }
print(paste0("Of ",nrow(groups_master)," lines, ",nrow(groups_master[!is.na(groups_master$group),])," have been assigend a group after the grid method, at ",Sys.time()))
return(groups_master)
}

#########################################################################


grouplines_complete <- function(lines_master, group_no, size_limit){

  groups_master =  data.frame(id=as.character(),nrow=as.integer())
  id2id = data.frame(id_old=as.character(lines_master$id),id_new=as.integer(1:nrow(lines_master)))
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
    lines <- lines_master[seq.int(from = v, to = nrow(lines_master), goes),] # Subset evenly across the country
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
      groups[groups$id==line1$id,2] <- group_no
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
          groups[groups$id==line2$id,2] <- group_no
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
    group_no = group_no + 1

  }
  close(pb)

  groups_master = rbind(groups_master, groups)
  }
  groups_master$id = as.integer(groups_master$id)
  groups_master = left_join(groups_master, id2id, by = c("id" = "id_new"))
  return(groups_master)

}

########################################################################################################

grouplines_GrdBear <- function(lines_master, group_no, grid_size, attempts){
  chunk_size = 10000
  #Groups lines quickly by will not find a group for all lines
  #Check if too many lines to do at once
  if(nrow(lines_master) < chunk_size){
    goes = 1
  } else {
    goes = ceiling(nrow(lines_master)/chunk_size)
  }
  #Set up df to store results
  groups_master = data.frame(id_old = as.character(lines_master$id), id_new = as.integer(1:nrow(lines_master)),bearing=as.integer(NA),grid = as.integer(NA) ,group=as.integer(NA))
  #rowsum_master = data.frame(id_new = as.integer(),count=as.integer())
  #Rename IDs so that there are list of intergers
  lines_master$id = groups_master$id_new

  # Stage 1: find the bearing for all the lines
  #Do bearings in chunks becuase fails over 10,000
  bearing_res <- data.frame(id_new = as.integer(), bearing = as.integer())
  for(v in 1:goes){
    if((v*chunk_size)>=nrow(lines_master)){l_max = nrow(lines_master)}else{l_max = (v*chunk_size)}
    lines = lines_master[(1 + (v-1)*chunk_size):l_max,]
    bearing <- round(line_bearing(lines),0)
    for(h in 1:length(bearing)){
      if(bearing[h]<=0){bearing[h]=bearing[h]+180}
      if(bearing[h]==0){bearing[h]=180} # first if intorduces some 0 so remove them
    }
    bearing <- data.frame(id_new = as.integer(lines$id), bearing = as.integer(bearing))
    bearing_res <- rbind(bearing_res,bearing)
  }
  groups_master$bearing <- bearing_res$bearing[match(groups_master$id_new, bearing_res$id_new)]
  print(paste0("Bearings found for all lines at ",Sys.time()))
  remove(bearing, bearing_res, lines, goes, v, h, l_max)

  # Stage 2: find the grid cell for each line
  lines_trans <- spTransform(lines_master, CRS("+proj=utm +zone=30 +datum=WGS84"))
  points = SpatialLinesMidPoints(lines_trans)

  ### define SpatialGrid object
  bb <- bbox(points)
  cs <- c(grid_size, grid_size)
  cc <- bb[, 1] + (cs/2)  # cell offset
  cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd <- SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)), proj4string=CRS(proj4string(points)))

  #Assign Grid IDs to Lines

  over <- over(points, sp_grd)
  groups_master$grid <- over$id
  remove("lines_trans","over","bb","cs","cc","cd","grd","sp_grd","points")

  #Stage 4: Perfom a Grid aware loop of bearings were each bearing has only one route per grid cell

  for(k in 1:attempts){
    groups = groups_master[is.na(groups_master$group),]
    if(nrow(groups) == 0){break()}
    for(j in 1:180){
      set = groups[groups$bearing == j,]
      set = set[!duplicated(set$grid), ]
      #id_list <- groups$id_new[groups_master$bearing == j]
      lines = lines_master[lines_master$id %in% set$id_new,]
      matrix_master = gIntersects(lines, byid = T)
      rownames(matrix_master) = lines$id
      colnames(matrix_master) = lines$id
      rowsum = data.frame(id_new = rownames(matrix_master),count=as.integer(rowSums(matrix_master)))
      #rowsum_master = rbind(rowsum_master,rowsum)
      twos = as.character(rowsum$id_new[rowsum$count <= 2])
      matrix = matrix_master[(rownames(matrix_master) %in% twos),(colnames(matrix_master) %in% twos), drop = F]
      rowsum2 = data.frame(id_new = rownames(matrix),count=as.integer(rowSums(matrix)))
      ones = as.character(rowsum2$id_new[rowsum2$count == 1])
      groups_master$group[groups_master$id_new %in% ones] <- group_no
      group_no = group_no + 1

    }
    print(paste0("Of ",nrow(groups_master)," lines, ",nrow(groups_master[!is.na(groups_master$group),])," have been assigend a group after attempt number ", k ,", at ",Sys.time()))
  }

  return(groups_master)
}