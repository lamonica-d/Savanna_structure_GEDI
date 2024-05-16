library(sf)
library(terra)
library(doParallel)
library(foreach)
library(Rmpi)

grid_of_distant_cells <- function(target_nrow,target_ncol,plot_grid=FALSE){
  
  # first sub-grid
  conserved1 <- rep(rep(c(TRUE,FALSE),each=target_ncol),times=target_nrow%/%2)
  if (target_nrow%%2==1){ conserved1 <- c(conserved1,rep(TRUE,times=target_ncol)) }
  
  # second sub-grid
  if (target_ncol%%2==0){ 
    conserved2 <- rep(rep(c(TRUE,FALSE),times=target_ncol%/%2),times=target_nrow)
  }
  if (target_ncol%%2==1){ 
    conserved2 <- rep(c(rep(c(TRUE,FALSE),times=target_ncol%/%2),TRUE),times=target_nrow)
  }
  
  # final sub-grid
  conserved = rep(TRUE,length(conserved1))
  for (s in 1:length(conserved)){
    if(conserved1[s] == FALSE | conserved2[s] == FALSE){conserved[s] = FALSE}
  }
  
  if (plot_grid==TRUE){
    test_conserved <- matrix(conserved,ncol=target_ncol,byrow=TRUE)
    print(paste(target_nrow,"x",target_ncol,"matrix"))
    print(paste("length(conserved) =",length(conserved)))
    print(test_conserved)
  }
  
  return(conserved)
}


intersect_custom <- function(extent_i, nc, specific_table){

  #intersect proprement dit
  temp <- st_intersects(nc, extent_i)
  
  #points inside
  pts_inside <- nc[which(lengths(temp)>0),]
  coord_pts_inside <- st_coordinates(pts_inside)
  df_pts_inside <- data.frame(index_point = pts_inside$index_point, x.meter = coord_pts_inside[,1],
                              y.meter = coord_pts_inside[,2])
  
  #merge pour recup les observations
  index_points <- merge(df_pts_inside,
                 subset(specific_table, select = -c(x,y)),
                 by = "index_point")
  
  #on renvoie
  return(index_points)
  
 }

#cell size (in meters)
cell <- 50000

#load all data table
specific_table <- readRDS(file = "data_6_ecoregion.RDS")

rownames(specific_table) = 1:nrow(specific_table)
specific_table <- cbind(
  1:nrow(specific_table),
  specific_table$x,
  specific_table$y,
  specific_table
)
colnames(specific_table)[1] = "index_point"
colnames(specific_table)[2] = "x_TRUE"
colnames(specific_table)[3] = "y_TRUE"
# then "x" and "y" columns can be modified as x_TRUE and y_TRUE are saved

table_new <- data.frame(
  index_point = specific_table$index,
  coordxTRUE = specific_table$x_TRUE,
  coordyTRUE = specific_table$y_TRUE,
  keep = rep(NA,nrow(specific_table))
)


#############################################################################
#1) la grille
meter_coord <- proj4::project(xy = table_new[,2:3], proj = "+proj=utm +datum=WGS84")
table_new2 <- data.frame(table_new, x.meter = meter_coord$x, y.meter = meter_coord$y)
new_spatvector <- terra::vect(table_new2[,c(1,4:6)], geom = c("x.meter", "y.meter"), crs = 3857) 

# get new_spatvector "window" : xmin, xmax, ymin, ymax
window <- terra::ext(new_spatvector)
dx <- geodist::geodist(x = c(window[1],window[3]), y = c(window[2],window[3]), measure = "haversine")
dy <- geodist::geodist(x = c(window[1],window[3]), y = c(window[1],window[4]), measure = "haversine")
(target_ncol <- round(dx/cell))
(target_nrow <- round(dy/cell))
# dimensions of y an z : 108, 333, 1  (nrow, ncol, nlyr)
y <- terra::rast(new_spatvector, ncols = target_ncol, nrows = target_nrow, nlyrs = 1)
values(y) <- grid_of_distant_cells(target_nrow,target_ncol)

#2) les cellules qu'on garde : une table avec coord du centre des cellules gardées
table_kept_cells <- data.frame(cbind(values(y),crds(y)))
table_kept_cells <- subset(table_kept_cells, lyr.1 == 1)
colnames(table_kept_cells) <- c("keep", "x.meter", "y.meter")

#3) pour chaque cellule de la table : calcul de l'extent
extent_list <- list()
for (i in 1:nrow(table_kept_cells)){
  x.random <- table_kept_cells$x.meter[i]
  y.random <- table_kept_cells$y.meter[i]
  extent_list[[i]] <- st_polygon(list(rbind(c(x.random - cell/2, y.random - cell/2),
                                          c(x.random + cell/2, y.random - cell/2), 
                                          c(x.random + cell/2, y.random + cell/2),
                                          c(x.random - cell/2, y.random + cell/2),
                                          c(x.random - cell/2, y.random - cell/2))))
}

#4) on intersect chaque extent (ie chaque ilot) avec le newspatvector 
##on recup les index_point des points qui sont dans l'ilot i
#&5) on merge pour recup les observations

#on passe le spatvector en sf
nc <- st_as_sf(new_spatvector, coords = c("x.meter", "y.meter"), crs = 3857)

numWorkers <- 45
extent_list <- extent_list[3001:3500]
extent_list_split <- lapply(.splitIndices(length(extent_list),numWorkers), function(i) extent_list[i])
save(extent_list_split, file = "extent_list_split_test.Rda") 

cl <- makeCluster(numWorkers, type = "MPI")
registerDoParallel(cl)
clusterCall(cl, function () Sys.info () [c ( "nodename", "machine" ) ] )
clusterExport(cl,"nc")
clusterExport(cl,"extent_list_split")
clusterExport(cl,"specific_table")
clusterEvalQ(cl, library(sf))

Sys.time()

index_points_list <- foreach(i=1:numWorkers)%dopar%{
 lapply(FUN = intersect_custom, X = extent_list_split[i], nc = nc, specific_table = specific_table)
 }
 
 Sys.time()

stopCluster(cl)
mpi.exit()

save(index_points_list, file = "index_points_list_test.Rda") 

