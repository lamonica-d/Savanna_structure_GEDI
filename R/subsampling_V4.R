### prépa des données pour nouveau test du modèle
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

library(terra)
library(sf)

######sous échantillonnage tous les x km
cell <- 10**5

specific_table <- readRDS(
  file.path( "transformed_data", paste0("data_pre_subsampling.RDS"))
) 

table_new <- data.frame(
  index_point = specific_table$index,
  coordxTRUE = specific_table$x_TRUE,
  coordyTRUE = specific_table$y_TRUE,
  keep = rep(NA,nrow(specific_table)),
  rh98 = specific_table$rh98,
  cc = specific_table$canopy_cover,
  clay_percent_std = specific_table$clay_percent_std,
  fire_freq_std = specific_table$fire_freq_std,
  prec_std = specific_table$prec_std
)

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

#!!!!!!!!
#ici on a des coord latlong et une taille de cellule en metres donc a gerer

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
nc <- st_as_sf(new_spatvector, coords = c("x.meter", "y.meter"), crs = 3857)

index_points_list <- list(length = nrow(table_kept_cells))
for (i in 1:nrow(table_kept_cells)){
  extent_list[[i]] -> extent_i
 print(paste("i",i,"/",nrow(table_kept_cells)))
 print(Sys.time())
  #intersect proprement dit
  temp <- st_intersects(nc, extent_i)
  
  #points inside
  pts_inside <- nc[which(lengths(temp)>0),]
  coord_pts_inside <- st_coordinates(pts_inside)
  df_pts_inside <- data.frame(index_point = pts_inside$index_point, x.meter = coord_pts_inside[,1],
                              y.meter = coord_pts_inside[,2])
  
  #merge pour recup les observations
  temp2 <- merge(df_pts_inside,
                 subset(table_new),
                 by = "index_point")
  
  #on range dans la list
  index_points_list[[i]] <- temp2
}

rm(temp2)
rm(temp)
rm(extent_list)
rm(meter_coord)
rm(specific_table)
rm(table_new2)
rm(df_pts_inside)
rm(nc)

saveRDS(index_points_list, 
        file.path("transformed_data_ilots", "index_point_test"))

###quick visual check
library(viridis)
ccol <- viridis(n=3)
plot(table_kept_cells$x.meter, table_kept_cells$y.meter)
for (i in 13:15){
       x.random <- table_kept_cells$x.meter[i]
       y.random <- table_kept_cells$y.meter[i]
       points(x.random - cell/2*(sin(theta)), y.random - cell/2*(cos(theta)),
              col = ccol[i-12], pch = 16)
       points(x.random + cell/2*(cos(theta)), y.random - cell/2*(sin(theta)),
              col = ccol[i-12], pch = 16)
       points(x.random + cell/2*(sin(theta)), y.random + cell/2*(cos(theta)),
              col = ccol[i-12], pch = 16)
       points(x.random - cell/2*(cos(theta)), y.random + cell/2*(sin(theta)),
              col = ccol[i-12], pch = 16)
       
   }
truc <- index_points_list[[13]]
points(truc$x.meter, truc$y.meter, pch = 17)
truc <- index_points_list[[14]]
points(truc$x.meter, truc$y.meter, pch = 1, col = "lightgreen")
truc <- index_points_list[[15]]
points(truc$x.meter, truc$y.meter, pch = 1, col = "lightpink")
