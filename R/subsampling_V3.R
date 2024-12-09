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

######sous échantillonnage tous les x km
cell <- 10**4

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

# we just keep index x_TRUE and y_TRUE in table_new
#1) setting resolution
# from data.frame to spatvector
new_spatvector <- terra::vect(table_new, geom = c("coordxTRUE", "coordyTRUE"), crs = "+proj=longlat +datum=WGS84") 
# get new_spatvector "window" : xmin, xmax, ymin, ymax
window <- terra::ext(new_spatvector)
dx <- geodist::geodist(x = c(window[1],window[3]), y = c(window[2],window[3]), measure = "haversine")
dy <- geodist::geodist(x = c(window[1],window[3]), y = c(window[1],window[4]), measure = "haversine")
target_ncol <- round(dx/cell)
# Guinean 10**4 cells : 133
target_nrow <- round(dy/cell)
# Guinean 10**4 cells : 108

# 133*108 = 14364
# dimensions of y an z : 108, 333, 1  (nrow, ncol, nlyr)

#2) resampling
y <- terra::rast(new_spatvector, ncol = target_ncol, nrow = target_nrow, nlyrs = 1)
terra::values(y) <- grid_of_distant_cells(target_nrow,target_ncol)

#variables
z_q50 <- terra::rasterize(new_spatvector, y, fun=quantile, probs = c(0.5), na.rm = T,field = c("rh98", "cc")
)
z_q90 <- terra::rasterize(new_spatvector, y, fun=quantile, probs = c(0.9), na.rm = T,field = c("rh98", "cc")
)
#predictors
z_clay <- terra::rasterize(new_spatvector, y, fun=mean, na.rm = T, field = "clay_percent_std")
z_fire <- terra::rasterize(new_spatvector, y, fun=mean, na.rm = T, field = "fire_freq_std")
z_prec <- terra::rasterize(new_spatvector, y, fun=mean, na.rm = T, field = "prec_std")
#to keep track of the index point to get climatic variables
z <- terra::rasterize(new_spatvector, y, fun=sample, size = 1, field = "index_point")

intermediate_table <- data.frame(cbind(terra::values(y),terra::crds(y),terra::values(z),
                                      terra::values(z_q50),terra::values(z_q90),
                                      terra::values(z_clay),terra::values(z_fire),terra::values(z_prec)))

colnames(intermediate_table)[1] = "keep"
colnames(intermediate_table)[2] = "x_center_cell"
colnames(intermediate_table)[3] = "y_center_cell"
colnames(intermediate_table)[5] = "rh98_q50"
colnames(intermediate_table)[6] = "cc_q50"
colnames(intermediate_table)[7] = "rh98_q90"
colnames(intermediate_table)[8] = "cc_q90"
colnames(intermediate_table)[9] = "clay_percent_mean"
colnames(intermediate_table)[10] = "fire_freq_mean"
colnames(intermediate_table)[11] = "precip_mean"

colnames(specific_table)[1] = "index_point"
# we add x_center_cell and y_center_cell to the specific_table thanks to index_point
trsf_data <- merge(intermediate_table,
                   subset(specific_table, select = -c(x,y)),
                   by = "index_point")

conserved_sub_table <- trsf_data[trsf_data$keep==1,]
conserved_sub_table <- subset(conserved_sub_table, select = -c(index_point,keep) )

# ### removing the "ejected" points in the sea and so on :
# print(paste("nrow(conserved_sub_table) =",nrow(conserved_sub_table)))
# print("goodbye points in the sea")
# dist_cc_TRUE <- as.numeric()
# for (i in 1:nrow(conserved_sub_table)){
#   dist_cc_TRUE[i] = (1/1000) * geodist::geodist(x = conserved_sub_table[i,1:2], y = conserved_sub_table[i, 3:4])
# }
# conserved_sub_table <- conserved_sub_table[which(dist_cc_TRUE < sqrt(2)*10),]
# print(paste("nrow(conserved_sub_table) =",nrow(conserved_sub_table)))
# ###

rm(specific_table)
rm(new_spatvector)
rm(y)
rm(z)
rm(z_clay)
rm(z_fire)
rm(z_prec)
rm(z_q50)
rm(z_q90)
rm(intermediate_table)
rm(table_new)
rm(trsf_data)
#rm(dist_cc_TRUE)

#add precipitation squared
conserved_sub_table <- cbind(conserved_sub_table, 
                             mean_precip_carre = (conserved_sub_table$precip_mean)^2)

#remove row with NA
conserved_sub_table <- conserved_sub_table[-which(is.na(conserved_sub_table$clay_percent)),]

#save
saveRDS(
  object = conserved_sub_table,
  file = file.path(
    "transformed_data",
    paste0("subsampled_v3_10000.RDS")
  )
)    
