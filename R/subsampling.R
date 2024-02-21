#subsampling of ecoregion tables
rm(list=ls())
# Getting the paths
source("paths.R")
# Libraries
library(fst)
library(sf)
library(terra)

table <- fst::read.fst(paste0(path_to_GEDI_raw_data,"/Guinean_forest-savanna.fst", sep=""))
# To replace the NA by zeros :
table[is.na(table[,"fire_freq"]),"fire_freq"] <- 0
set.seed(1234)
sub_table <- table[sample(1:nrow(table),10**4, replace = F),]
raster <- rast(sub_table[,c(1:3,5,6,9)], type = "xyz", crs = "EPSG:4326")
#calcul distance between no NA cells to check taht there are further appart than 5 km
