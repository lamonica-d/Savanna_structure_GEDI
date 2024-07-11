# Cleaning the environment
rm(list=ls())
# Getting the paths
getwd()
source("paths.R")
# Libraries
library(terra)
library(sf)
#######################

save_rds_files = TRUE
save_coords_TRUE_geojson = TRUE
print_i_j_stuff = TRUE

big_cells_center <- readRDS(
                            file.path(
                                      path_to_Savanna_structure_GEDI_folder,
                                      "subsampling30avril",
                                      "big_cell_300km_center_cells.RDS")
                            )

table <- readRDS(
                 file.path(
                           path_to_Savanna_structure_GEDI_folder,
                           "subsampling30avril",
                           "subsampling_30avril.RDS")
                 )


rownames(table) <- 1:nrow(table)

table <- cbind(rep(NA,nrow(table)),rep(NA,nrow(table)),rep(NA,nrow(table)),table)
colnames(table)[1] <- "near_the_big_cell"
colnames(table)[2] <- "x_center_big_cell"
colnames(table)[3] <- "y_center_big_cell"

for (u in 1:nrow(table)){
#for (u in 1:100){
    
  if(print_i_j_stuff==TRUE){ print(paste("u",u,"/",nrow(table))) }
  
  list_dist = c()
  list_v = c()
  
  for(v in 1:nrow(big_cells_center)){
    
    dist <- geodist::geodist(x = table[u,c("x_TRUE","y_TRUE")],
                             y = big_cells_center[v,c("x_center_big_cell","y_center_big_cell")],
                             quiet=TRUE)
    
    if(dist<sqrt(2)*170*10**3){
      list_dist = c(list_dist,dist)
      list_v = c(list_v,v)
      }
    
  }
  
  # print(list_dist)
  
  if(length(list_dist)>0){
    closer_big_cell_v = list_v[which.min(list_dist)]
    
    table[u,"near_the_big_cell"] <- big_cells_center[closer_big_cell_v,"big_cell_number"]
    table[u,"x_center_big_cell"] <- big_cells_center[closer_big_cell_v,"x_center_big_cell"]
    table[u,"y_center_big_cell"] <- big_cells_center[closer_big_cell_v,"y_center_big_cell"]
    }


}

colSums(is.na(table))

if (save_rds_files ==TRUE){
  saveRDS(
    object = table,
    file = file.path(
      path_to_Savanna_structure_GEDI_folder,
      "subsampling30avril",
      paste0("final_table_10km_associated_to_300km_cell.RDS")
    )
  )    
  
}

if (save_coords_TRUE_geojson == TRUE){
  
  sf_obj2 <- st_as_sf(table,coords = c("x_TRUE", "y_TRUE"),crs = 4326)
  
  st_write(
    sf_obj2,
    file.path(path_to_Savanna_structure_GEDI_folder,
              "subsampling30avril",
              paste0("final_table_TRUE_positions_10km_associated_to_300km_cell.geojson")
    ), delete_dsn = T
  )
  
}


if (save_coords_TRUE_geojson == TRUE){
  
  sf_obj2 <- st_as_sf(table,coords = c("x_center_cell", "y_center_cell"),crs = 4326)
  
  st_write(
    sf_obj2,
    file.path(path_to_Savanna_structure_GEDI_folder,
              "subsampling30avril",
              paste0("final_table_center_cells_10km_associated_to_300km_cell_small_center_cells.geojson")
    ), delete_dsn = T
  )
  
}

if (save_coords_TRUE_geojson == TRUE){
  
  sf_obj3 <- st_as_sf(table,coords = c("x_center_big_cell", "y_center_big_cell"),crs = 4326)
  
  st_write(
    sf_obj3,
    file.path(path_to_Savanna_structure_GEDI_folder,
              "subsampling30avril",
              paste0("final_table_big_center_cells_10km_associated_to_300km_cell_small_center_cells.geojson")
    ), delete_dsn = T
  )
  
}



