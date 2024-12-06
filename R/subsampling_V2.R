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


#1) load table
complete_table <- readRDS(file = file.path(
    "rawdata_post_preprocessing",
    "6_ecoregions_without_duplicate_standardized_ONLY_over_6_ecoregions.RDS"
  )
)

#2) remove fire_frq < 1/20
complete_table_1_0 <- subset(complete_table, fire_freq > 1/20)
rm(complete_table)

#2bis) remove rh98 < 3
complete_table_1 <- subset(complete_table_1_0, rh98 > 3)
rm(complete_table_1_0)

#3) get soil info & intersect & standardized
soil_db <- rast(file.path("rawdata","soil_af_isda",
                          "isda_clay.tot.psa_0-20cm_v0.13_30s.tif"))
test <- terra::extract(soil_db, complete_table_1[,1:2])
rm(soil_db)
colnames(test)[2] <- "clay_percent"
test_std <- (test$clay_percent - mean(test$clay_percent, na.rm = T))/sd(test$clay_percent, na.rm = T)
complete_table_1 <- data.frame(complete_table_1, clay_percent = test$clay_percent,
                               clay_percent_std = test_std)
rm(test)

#4) intersect to remove loc where > 10 hab/km2
pop_data <- rast("rawdata/AFR_PPP_2000_adj_v2.tif")
test <- terra::extract(pop_data, complete_table_1[,1:2])
rm(pop_data)
colnames(test)[2] <- "pop_density"
complete_table_1 <- data.frame(complete_table_1, pop_density = test$pop_density)
rm(test)
specific_table <- subset(complete_table_1, pop_density <= 10)
rm(complete_table_1)

######sous échantillonnage tous les x km
cell <- 10**4

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

table_new <- data.frame(
  index_point = specific_table$index,
  coordxTRUE = specific_table$x_TRUE,
  coordyTRUE = specific_table$y_TRUE,
  keep = rep(NA,nrow(specific_table))
)

# we just keep index x_TRUE and y_TRUE in table_new
#1) setting resolution
# from data.frame to spatvector
new_spatvector <- terra::vect(table_new, geom = c("coordxTRUE", "coordyTRUE"), crs = "+proj=longlat +datum=WGS84") 
# get new_spatvector "window" : xmin, xmax, ymin, ymax
window <- terra::ext(new_spatvector)
dx <- geodist::geodist(x = c(window[1],window[3]), y = c(window[2],window[3]), measure = "haversine")
dy <- geodist::geodist(x = c(window[1],window[3]), y = c(window[1],window[4]), measure = "haversine")
(target_ncol <- round(dx/cell))
# Guinean 10**4 cells : 133
(target_nrow <- round(dy/cell))
# Guinean 10**4 cells : 108

# 133*108 = 14364
# dimensions of y an z : 108, 333, 1  (nrow, ncol, nlyr)

#2) resampling
y <- terra::rast(new_spatvector, ncol = target_ncol, nrow = target_nrow, nlyr = 1)
terra::values(y) <- grid_of_distant_cells(target_nrow,target_ncol)

z <- terra::rasterize(new_spatvector, y, fun=sample, size = 1, field = "index_point")
# fun = sample , size = 1 to get one random sample in the cell

intermediate_table = data.frame(cbind(terra::values(y),terra::crds(y), terra::values(z)))
colnames(intermediate_table)[1] = "keep"
colnames(intermediate_table)[2] = "x_center_cell"
colnames(intermediate_table)[3] = "y_center_cell"

# nrow(crds(y)) 35964
# length(values(z)) 35964 ok

colnames(specific_table)[1] = "index_point"
# we add x_center_cell and y_center_cell to the specific_table thanks to index_point
trsf_data <- merge(intermediate_table,
                   subset(specific_table, select = -c(x,y)),
                   by = "index_point")

conserved_sub_table <- trsf_data[trsf_data$keep==1,]
conserved_sub_table <- subset(conserved_sub_table, select = -c(index_point,keep) )

### removing the "ejected" points in the sea and so on :
print(paste("nrow(conserved_sub_table) =",nrow(conserved_sub_table)))
print("goodbye points in the sea")
dist_cc_TRUE <- as.numeric()
for (i in 1:nrow(conserved_sub_table)){
  dist_cc_TRUE[i] = (1/1000) * geodist::geodist(x = conserved_sub_table[i,1:2], y = conserved_sub_table[i, 3:4])
}
conserved_sub_table <- conserved_sub_table[which(dist_cc_TRUE < sqrt(2)*10),]
print(paste("nrow(conserved_sub_table) =",nrow(conserved_sub_table)))
### 

rm(specific_table)
rm(new_spatvector)
rm(y)
rm(z)
rm(intermediate_table)
rm(table_new)
rm(trsf_data)
rm(dist_cc_TRUE)

#renaming columns
colnames(conserved_sub_table)[13:15] <- c("fire_freq_std", 
                                   "mean_precip_std",
                                   "mean_temp_std")
conserved_sub_table <- conserved_sub_table[,-12]
conserved_sub_table <- cbind(conserved_sub_table, 
                      mean_precip_carre = (conserved_sub_table$mean_precip_std)^2)

#save
saveRDS(
  object = conserved_sub_table,
  file = file.path(
    "transformed_data",
    paste0("subsampled_6_ecoregions_rh98sup3.RDS")
  )
)    
