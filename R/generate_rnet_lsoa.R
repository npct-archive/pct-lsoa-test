#Generate Rout Networks
#libs
library(rmapshaper)
library(stplanr)
library(leaflet)

# Load Data 
rf <- readRDS("data/rf_LSOA_Cam2.Rds")
lines <- readRDS("data/Lines_LSOA_Cam.Rds")
scens <- c("govtarget_slc", "gendereq_slc", "dutch_slc", "ebike_slc")

#Simplify
rf_simp <- rf
rf_simp@data <- cbind(rf_simp@data, lines@data[c("bicycle", scens)])
#rft@data <- cbind(rft@data, l@data[c("bicycle", scens)])
rf_simp <- ms_simplify(input = rf_simp, keep = 0.1, method = "dp", keep_shapes = TRUE, no_repair = FALSE, snap = TRUE)

rnet <- overline(rf_simp, "bicycle")
saveRDS(rnet,file = "../pct-lsoa-test/data/rnet_LSOA_Cam.Rds")

if(require(foreach) & require(doParallel)){
  n_cores <- 4 # set max number of cores to 4
  # reduce n_cores - uncomment for 2 core machines 
  # if(parallel:::detectCores() < 4)
  #   n_cores <- parallel:::detectCores()
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  # foreach::getDoParWorkers()
  # create list in parallel
  rft_data_list <- foreach(i = scens) %dopar% {
    rnet_tmp <- stplanr::overline(rft, i)
    rnet_tmp@data[i]
  }
  # save the results back into rnet with normal for loop
  for(j in seq_along(scens)){
    rnet@data <- cbind(rnet@data, rft_data_list[[j]])
  }
  stopCluster(cl = cl)
} else {
  for(i in scens){
    rnet_tmp <- overline(rft, i)
    rnet@data[i] <- rnet_tmp@data[i]
    rft@data[i] <- NULL
  }
}
