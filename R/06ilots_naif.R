# Cleaning the environment
rm(list=ls())
# Getting the paths
getwd()
source("paths.R")
# Setting the current path
path_to_R_folder = file.path(
  path_to_Savanna_structure_GEDI_folder,
  "R"
)
# Libraries
library(terra)
library(sf)
#######################

save_rds_files = TRUE
save_coords_TRUE_geojson = TRUE
print_i_j_stuff = TRUE

full_table <- readRDS(file.path(path_to_Savanna_structure_GEDI_folder,"rawdata_post_preprocessing","complete_corresponding_table_without_duplicate.RDS"))

files <- list.files(path=file.path(path_to_Savanna_structure_GEDI_folder,"transformed_data"))
files <- files[grep("\\.RDS$", files)]
files <- files[-1] # enlever all.Africa
files

i = 1
for(i in 1:length(files)){
  table <- readRDS(file.path(path_to_Savanna_structure_GEDI_folder,"transformed_data",files[i]))
  # just for test :
  # table <- table[1:10,]
  #
  name <- substr(files[i], 1, nchar(files[i]) - 4)
  big_table <- full_table[full_table[,"ecoregion"]==name,]
  rownames(big_table) <- 1:nrow(big_table)
  big_table <- cbind(rep(NA,nrow(big_table)),big_table)
  colnames(big_table)[1] <- "near_a_center_cell"
  # # just for test :
  # big_table <- big_table[1:100,]
  # #
  
  for (u in 1:nrow(table)){
    if(print_i_j_stuff==TRUE & u %%1 == 0 ){
      print(paste(name,"u",u))
      print(Sys.time())
      }
    for (v in 1:nrow(big_table)){
      
      dist <- geodist::geodist(x = table[u,c("x_center_cell","y_center_cell")], y = big_table[v,c("x","y")],quiet=TRUE)
      if(dist<sqrt(2)*10**4){big_table[v,"near_a_center_cell"] <- u}else{big_table[v,"near_a_center_cell"] <- NA}
    }
  }
  
  if (save_rds_files ==TRUE){
    saveRDS(
      object = big_table,
      file = file.path(
        path_to_Savanna_structure_GEDI_folder,
        "transformed_data_ilots",
        paste0(name,"_ilots.RDS")
      )
    )    
    
    print(paste(paste0(name,".RDS"),"DONE"))
    
  }
  
  if (save_coords_TRUE_geojson == TRUE){
    
    sf_obj2 <- st_as_sf(big_table,coords = c("x", "y"),crs = 4326)
    
    st_write(
      sf_obj2,
      file.path(path_to_Savanna_structure_GEDI_folder,
                "transformed_data_ilots",
                paste0(name,"_TRUE_positions_ilots.geojson")
      ), delete_dsn = T
    )
    
    print(paste(paste0(name,"_TRUE_positions_ilots.geojson"),"DONE"))
  }
}



