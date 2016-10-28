# Aim: find and re-add 'missing lines'
source("../pct-load/set-up.R")
cents = geojsonio::geojson_read("../pct-lsoa-test/data/lsoa_centroids.geojson", what = "sp")
# load OD data - source http://wicid.ukdataservice.ac.uk/
unzip("../pct-lsoa-test/data/WM12EW[CT0489]_lsoa.zip")
flow_cens = readr::read_csv("WM12EW[CT0489]_lsoa.csv")
file.remove("WM12EW[CT0489]_lsoa.csv")
nrow(flow_cens) # 7.74 m

# subset the centroids for testing (comment to generate national data)

cents = cents[grep(pattern = "Camb", x = cents$name),]
#plot(cents)
nrow(cents) #32844
o <- flow_cens$`Area of usual residence` %in% cents$code
d <- flow_cens$`Area of Workplace` %in% cents$code
flow <- flow_cens[o & d, ] # subset OD pairs with o and d in study area
nrow(flow) #7.28 m

omatch = match(flow$`Area of usual residence`, cents$code)
dmatch = match(flow$`Area of Workplace`, cents$code)

cents_o = cents@coords[omatch,]
cents_d = cents@coords[dmatch,]
summary(is.na(cents_o)) # check how many origins don't match
summary(is.na(cents_d))
geodist = geosphere::distHaversine(p1 = cents_o, p2 = cents_d) / 1000 # assign euclidean distanct to lines (could be a function in stplanr)
summary(is.na(geodist))

hist(geodist, breaks = 0:800)
flow$dist = geodist
flow = flow[!is.na(flow$dist),] # there are 36k destinations with no matching cents - remove
#flow = flow[flow$dist >= 20,] # subset based on euclidean distance
#flow = flow[flow$dist < 5,]
names(flow) = gsub(pattern = " ", "_", names(flow))
flow_twoway = flow
flow = onewayid(flow, attrib = 5:256, id1 = "Area_of_usual_residence", id2 = "Area_of_Workplace")
#checked upto this point

flow[1:2] = cbind(pmin(flow[[1]], flow[[2]]), pmax(flow[[1]], flow[[2]]))

nrow(flow) # down to 0.9m, removed majority of lines
cents <- cents[,c(2,1)] # switch column order for the od2line function to work
lines = od2line2(flow = flow, zones = cents)

plot(lines)

class(lines)
length(lines)
lines = SpatialLinesDataFrame(sl = lines, data = flow)
names(lines)
proj4string(lines) = CRS("+init=epsg:4326") # set crs

sum(lines$`AllMethods_AllSexes_Age16Plus`)
summary(lines$`AllMethods_AllSexes_Age16Plus`)

# to be removed when this is in stplanr
od_dist <- function(flow, zones){
  omatch = match(flow[[1]], cents@data[[1]])
  dmatch = match(flow[[2]], cents@data[[1]])
  cents_o = cents@coords[omatch,]
  cents_d = cents@coords[dmatch,]
  geosphere::distHaversine(p1 = cents_o, p2 = cents_d)
}

lines$dist = od_dist(flow = lines@data, zones = cents) / 1000

summary(lines$dist)

lines@data <- dplyr::rename(lines@data,
                        lsoa1 = Area_of_usual_residence,
                        lsoa2 = Area_of_Workplace,
                        all = `AllMethods_AllSexes_Age16Plus`,
                        bicycle = Bicycle_AllSexes_Age16Plus#,
                        #train = Train,
                        #bus = `Bus,_minibus_or_coach`,
                        #car_driver = `Driving_a_car_or_van`,
                        #car_passenger = `Passenger_in_a_car_or_van`,
                        #foot = On_foot,
                        #taxi = Taxi,
                        #motorbike = `Motorcycle,_scooter_or_moped`,
                        #light_rail = `Underground,_metro,_light_rail,_tram`,
                        #other = Other_method_of_travel_to_work
)

lines$WorkAtHome_AllSexes_Age16Plus <- NULL

names(lines)
# Save the Lines
saveRDS(lines,file = "../pct-lsoa-test/data/Lines_Cam.Rds")

# generate the fastest routes
rf = line2route(l = lines, route_fun = route_cyclestreet, plan = "fastest", base_url = "http://pct.cyclestreets.net/api/")
saveRDS(rf,file ="../pct-lsoa-test/data/rf_LSOA_Cam2.Rds")

rq = line2route(l = lines, route_fun = route_cyclestreet, plan = "quietest", base_url = "http://pct.cyclestreets.net/api/")
saveRDS(rq,file ="../pct-lsoa-test/data/rq_LSOA_Cam2.Rds")

