library(terra)
library(sf)
library(proj4)
library(geodist)
library(doParallel)
library(foreach)

grid_of_distant_cells <- function(target_nrow,target_ncol,plot_grid=FALSE){
  
  # first sub-grid
  conserved1 <- rep(rep(c(FALSE, TRUE),each=target_ncol),times=target_nrow%/%2)
  if (target_nrow%%2==1){ conserved1 <- c(conserved1,rep(FALSE,times=target_ncol)) }
  
  # second sub-grid
  if (target_ncol%%2==0){ 
    conserved2 <- rep(rep(c(FALSE,TRUE),times=target_ncol%/%2),times=target_nrow)
  }
  if (target_ncol%%2==1){ 
    conserved2 <- rep(c(rep(c(FALSE,TRUE),times=target_ncol%/%2),FALSE),times=target_nrow)
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

select_points_inside <- function(extent_list, nc, table_new, table_kept_cells){
  df_all <- data.frame()
  
  for (i in 1:length(extent_list)){
    extent_list[[i]] -> extent_i
    #intersect proprement dit
    temp <- st_intersects(nc, extent_i)
    
    #points inside
    pts_inside <- nc[which(lengths(temp)>0),]
    if (nrow(pts_inside)>0){
      coord_pts_inside <- st_coordinates(pts_inside)
      df_pts_inside <- data.frame(index_point = pts_inside$index_point, x.meter = coord_pts_inside[,1],
                                  y.meter = coord_pts_inside[,2], cell_id = i,  
                                  x.center = table_kept_cells$x.meter[i],
                                  y.center = table_kept_cells$y.meter[i])
      
      #merge pour recup les observations
      temp2 <- merge(df_pts_inside,
                     subset(table_new),
                     by = "index_point")
      
      #on range dans un df
      df_all <- rbind(df_all, temp2)
    }else{
      next
    }
  }
  return(df_all)
}

######sous échantillonnage tous les x metres
cell <- 10**4
##choix de la classe de precip
q = 1

#specific_table <- readRDS("data_pre_subsampling.RDS")
specific_table <- readRDS(file.path("transformed_data", paste0("data_classe_precip", 1, ".RDS")))

table_new <- data.frame(
  index_point = specific_table$index,
  coordxTRUE = specific_table$x_TRUE,
  coordyTRUE = specific_table$y_TRUE,
  rh98 = specific_table$rh98,
  cc = specific_table$canopy_cover,
  clay_percent_std = specific_table$clay_percent_std,
  fire_freq_std = specific_table$fire_freq_std,
  prec_std = specific_table$prec_std
)

rm(specific_table)

#1) la grille
new_spatvector <- terra::vect(table_new, geom = c("coordxTRUE", "coordyTRUE"), crs = "+proj=longlat +datum=WGS84") 
# get new_spatvector "window" : xmin, xmax, ymin, ymax
window <- terra::ext(new_spatvector)
#input lat long, output meters
dx <- geodist::geodist(x = c(window[1],window[3]), y = c(window[2],window[3]), measure = "haversine")
dy <- geodist::geodist(x = c(window[1],window[3]), y = c(window[1],window[4]), measure = "haversine")
target_ncol <- round(dx/cell)
# Guinean 10**4 cells : 133
target_nrow <- round(dy/cell)

# dimensions of y an z : 108, 333, 1  (nrow, ncol, nlyr)
y <- terra::rast(new_spatvector, ncols = target_ncol, nrows = target_nrow, nlyrs = 1)
values(y) <- grid_of_distant_cells(target_nrow,target_ncol)

#2) les cellules qu'on garde : une table avec coord du centre des cellules gardées
table_kept_cells <- data.frame(cbind(values(y),crds(y)))
table_kept_cells <- subset(table_kept_cells, lyr.1 == 1)
colnames(table_kept_cells) <- c("keep", "x.center", "y.center")

#3) pour chaque cellule de la table : calcul de l'extent
#ici on a des coord latlong et une taille de cellule en metres donc a gerer

#a) on proj avec une projection qui conserve les distances dans la zone 
# https://projectionwizard.org/ 
#+proj=eqc +lon_0=15.46875 +lat_ts=15.3756389 +datum=WGS84 +units=m +no_defs
meter_coord <- proj4::project(xy = table_new[,2:3], 
                              proj = "+proj=eqc +lon_0=15.46875 +lat_ts=15.3756389 +datum=WGS84 +units=m +no_defs")
table_new2 <- data.frame(table_new, x.meter = meter_coord$x, y.meter = meter_coord$y)
new_spatvector <- terra::vect(table_new2[,c(1,9,10)], geom = c("x.meter", "y.meter"),
                              crs = "+proj=eqc +lon_0=15.46875 +lat_ts=15.3756389 +datum=WGS84 +units=m +no_defs") 
meter_coord2 <- proj4::project(xy = table_kept_cells[,2:3], 
                               proj = "+proj=eqc +lon_0=15.46875 +lat_ts=15.3756389 +datum=WGS84 +units=m +no_defs")
table_kept_cells <- cbind(table_kept_cells, x.meter = meter_coord2$x, y.meter = meter_coord2$y)

#find extents
extent_list <- list()
for (i in 1:nrow(table_kept_cells)){
  x.center <- table_kept_cells$x.meter[i]
  y.center <- table_kept_cells$y.meter[i]
  extent_list[[i]] <- st_polygon(list(rbind(
    c(x.center - cell/2, y.center - cell/2),
    c(x.center + cell/2, y.center - cell/2),
    c(x.center + cell/2, y.center + cell/2),
    c(x.center - cell/2, y.center + cell/2),
    c(x.center - cell/2, y.center - cell/2)
  )))
}

#4) on intersect chaque extent (ie chaque ilot) avec le newspatvector 
##on recup les index_point des points qui sont dans l'ilot i
#&5) on merge pour recup les observations

#on passe le spatvector en sf
nc <- st_as_sf(new_spatvector, coords = c("x.meter", "y.meter"), 
               crs = "+proj=eqc +lon_0=15.46875 +lat_ts=15.3756389 +datum=WGS84 +units=m +no_defs")

#version parallel
nb_cpu <- 20
extent_list_parallel <- lapply(splitIndices(length(extent_list),nb_cpu),
                               function(i) extent_list[i])

Sys.time()

cl <- makeCluster(nb_cpu)

registerDoParallel(cl)
clusterCall(cl, function () Sys.info () [c ( "nodename", "machine" ) ] )

clusterExport(cl,c("extent_list_parallel", "nc", "table_new", "table_kept_cells"))
clusterEvalQ(cl,library(doParallel))
clusterEvalQ(cl,library(foreach))
clusterEvalQ(cl,library(iterators))
clusterEvalQ(cl,library(sf))
clusterEvalQ(cl,library(terra))

test1 <- foreach(i=1:nb_cpu, .inorder = T) %dopar%{
  select_points_inside(extent_list_parallel[[i]], nc = nc, table_new = table_new, table_kept_cells = table_kept_cells)
}

stopCluster(cl)

Sys.time()

rm(temp2)
rm(temp)
rm(extent_list)
rm(meter_coord)
rm(table_new2)
rm(df_pts_inside)
rm(nc)

saveRDS(test1, paste0("data_point_in_cells_104_class_precip_",q))
