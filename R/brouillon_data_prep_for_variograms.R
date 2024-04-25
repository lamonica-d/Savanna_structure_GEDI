library(sf)

#cell size (in meters)
cell <- 50000

#load all data table
table <- readRDS(file = "rawdata/complete_corresponding_table_without_duplicate.RDS")
#select one ecoregion
(unique(table[,"ecoregion"]))

specific_table <- table[table[,"ecoregion"] == "Guinean_forest-savanna", ]
rm(table)

rownames(specific_table) = 1:nrow(specific_table)
specific_table <- cbind(
  1:nrow(specific_table),
  specific_table$x,
  specific_table$y,
  specific_table
)
colnames(specific_table)[1] = "index"
colnames(specific_table)[2] = "x_TRUE"
colnames(specific_table)[3] = "y_TRUE"
# then "x" and "y" columns can be modified as x_TRUE and y_TRUE are saved

# table_new <- data.frame(
#   index_point = specific_table$index,
#   coordxTRUE = specific_table$x_TRUE,
#   coordyTRUE = specific_table$y_TRUE,
#   keep = rep(NA,nrow(specific_table))
# )

#############################################################################
##### first test
new_spatvector <- terra::vect(table_new, geom = c("coordxTRUE", "coordyTRUE"), crs = "+proj=longlat +datum=WGS84") 
# get new_spatvector "window" : xmin, xmax, ymin, ymax
window <- terra::ext(new_spatvector)
dx <- geodist::geodist(x = c(window[1],window[3]), y = c(window[2],window[3]), measure = "haversine")
dy <- geodist::geodist(x = c(window[1],window[3]), y = c(window[1],window[4]), measure = "haversine")
(target_ncol <- round(dx/cell))
(target_nrow <- round(dy/cell))
# dimensions of y an z : 108, 333, 1  (nrow, ncol, nlyr)

#2) resampling
y <- terra::rast(new_spatvector, ncol = target_ncol, nrow = target_nrow, nlyr = 1)
#values(y) <- grid_of_distant_cells(target_nrow,target_ncol)

function_keeping_point <- function(x){sample(x, size = min(length(x),5), replace = F)}

z <- terra::rasterize(new_spatvector, y, fun=function_keeping_point, field = "index_point")



#############################################################################
###second test
table_select <- table_new
#transform lon/lat in meters
meter_coord <- proj4::project(xy = table_select[,2:3], proj = "+proj=utm +datum=WGS84")
table_select <- data.frame(table_select,x.meter = meter_coord$x, y.meter = meter_coord$y)
#from data.frame to spatvector
x <- terra::vect(table_select[,c(1,5,6)], geom = c("x.meter", "y.meter"), crs = "+proj=utm +datum=WGS84") 
#get x "window" : xmin, xmax, ymin, ymax
x_ext <- terra::ext(x)
#set a cell length, in meter
cell <- 20000
# dx <- geodist::geodist(x = c(x_ext[1],x_ext[3]), y = c(x_ext[2],x_ext[3]), measure = "haversine")
# dy <- geodist::geodist(x = c(x_ext[1],x_ext[3]), y = c(x_ext[1],x_ext[4]), measure = "haversine")
# (target_ncol <- round(dx/cell))
# (target_nrow <- round(dy/cell))

x.random <- runif(n = 1, min = x_ext[1], max = x_ext[2])
y.random <- runif(n = 1, min = x_ext[3], max = x_ext[4])
random_extent <- terra::ext(c(x.random - cell/2, x.random + cell/2, y.random - cell/2, y.random + cell/2))

# Sys.time()
# truc <- intersect(x, random_extent) #problème ici trop long
# Sys.time()


#############################################################################
###third test
table_select <- data.frame(
  index_point = specific_table$index,
  x_TRUE = specific_table$x_TRUE,
  y_TRUE = specific_table$y_TRUE
)

###version 1
# # Turn it into a rough metre-length system by projection  ?????
nc <- st_as_sf(table_select, coords = c("x_TRUE", "y_TRUE"), crs = 4326) # "+proj=longlat +datum=WGS84")
x_sf <- st_transform(nc, 3857)
# # and make a 5km buffer
# ncbuff = st_buffer(nc, 5000)


###version 2
#transform lon/lat in meters
meter_coord <- proj4::project(xy = table_select[,2:3], proj = "+proj=utm +datum=WGS84")
table_select2 <- data.frame(table_select,x.meter = meter_coord$x, y.meter = meter_coord$y)
# #from data.frame to spatvector
# x <- terra::vect(table_select[,c(1,5,6)], geom = c("x.meter", "y.meter"), crs = "+proj=utm +datum=WGS84") 
# #get x "window" : xmin, xmax, ymin, ymax
x_sf_v2 <- st_as_sf(table_select2[,c(1,4,5)], coords = c("x.meter", "y.meter"), crs = "+proj=utm +datum=WGS84") 

####on reprend là
x_ext <- st_bbox(x_sf)
#set a cell length, in meter
cell <- 100000
set.seed(5)
x.random <- runif(n = 1, min = x_ext[1], max = x_ext[3])
y.random <- runif(n = 1, min = x_ext[2], max = x_ext[4])
random_extent <- st_polygon(list(rbind(c(x.random - cell/2, y.random - cell/2),
                                       c(x.random + cell/2, y.random - cell/2), 
                                       c(x.random + cell/2, y.random + cell/2),
                                       c(x.random - cell/2, y.random + cell/2),
                                       c(x.random - cell/2, y.random - cell/2))))


truc <- st_intersects(x_sf, random_extent)
#truc_logical <- lengths(truc)>0
#pts_inside <- truc[truc_logical==T]

###ici il faut recup les coord de x_sf qui sont en mètres
pts_inside <- x_sf[which(lengths(truc)>0),]     #table_select[which(lengths(truc)>0),]
##ici il faut recup les coord sous forme de deux colonnes
coord_pts_inside <- st_coordinates(pts_inside)
df_pts_inside <- data.frame(index_point = pts_inside$index_point, x.meter = coord_pts_inside[,1],
                            y.meter = coord_pts_inside[,2])

colnames(specific_table)[1] = "index_point"
truc2 <- merge(df_pts_inside,
                   subset(specific_table, select = -c(x,y)),
                   by = "index_point")


var_rh98 <- geoR::variog(coords = data.frame(truc2$x.meter, truc2$y.meter), data = truc2$rh98)
plot(var_rh98)
