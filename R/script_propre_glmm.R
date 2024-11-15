
# Cleaning the environment
rm(list=ls())

# Libraries
library(fst)
library(rjags)
library(rstan)
stan_version()
library(stringr) 
library(brms)

# Guinean
load(file=file.path(path_to_ilots_dataRDA,'index_points_list_congo.Rda'))
data <- index_points_list

length(guinean)
table = as.data.frame(
  matrix(nrow=0,
         ncol=ncol(guinean[[1]])+1
  ))

colnames(table) <- c("indice_case",colnames(data[[1]]))

for (i in 1:length(guinean)){
  if(i%%1 == 0){print(i)}
  subtable <- cbind( rep(i,times=nrow(data[[i]])), data[[i]] )
  table <- rbind(table,subtable)
}

colnames(table)[1] = "indice_case"
colnames(table)
head(table,5)

saveRDS(
  object = table,
  file = file.path(
    path_to_Savanna_structure_GEDI_folder,
    "transformed_data_ilots",
    paste0("congo_ilots",".RDS")
  )
)    

sampled <- table[ sample(1:nrow(table),size=10**4,replace=FALSE) , ]
rownames(sampled) <- 1:10**4

require(sf)
sf_obj <- st_as_sf(sampled,coords = c("x_TRUE", "y_TRUE"),crs = 4326)
  
st_write(
  sf_obj,
  file.path(path_to_Savanna_structure_GEDI_folder,
            "transformed_data_ilots",
            paste0("congo_ilots.geojson")
  ), delete_dsn = T
)
  
guinean <- readRDS(file.path(path_to_Savanna_structure_GEDI_folder,"transformed_data_ilots","guinean_ilots.RDS"))
table(guinean$indice_case)

guinean <- guinean[guinean[,"indice_case"]<=100,]
table(guinean$indice_case)

sf_obj <- st_as_sf(guinean,coords = c("x_TRUE", "y_TRUE"),crs = 4326)

st_write(
  sf_obj,
  file.path(path_to_Savanna_structure_GEDI_folder,
            "transformed_data_ilots",
            paste0("test.geojson")
  ), delete_dsn = T
)


