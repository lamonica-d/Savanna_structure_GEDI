set.seed(1234)
rm(list=ls())
source("paths.R")
require(terra)
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

cell <- 10**4
name = c("Guinean_forest-savanna")
# complete_table = readRDS(file = file.path(
#   path_to_Savanna_structure_GEDI_folder,
#   "rawdata_post_preprocessing",
#   "complete_corresponding_table_without_duplicate_standardized.RDS"
# )
# )

specific_table <- complete_table[complete_table[,"ecoregion"] == name, ]
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
(target_nrow <- round(dy/cell))

#2) resampling
y <- terra::rast(new_spatvector, ncol = target_ncol, nrow = target_nrow, nlyr = 1)
values(y) <- grid_of_distant_cells(target_nrow,target_ncol)

z <- terra::rasterize(new_spatvector, y, fun=sample, size = 1, field = "index_point")

intermediate_table = data.frame(cbind(values(y),crds(y), values(z)))
colnames(intermediate_table)[1] = "keep"
colnames(intermediate_table)[2] = "x_center_cell"
colnames(intermediate_table)[3] = "y_center_cell"

colnames(specific_table)[1] = "index_point"

trsf_data <- merge(intermediate_table,
                   subset(specific_table, select = -c(x,y)),
                   by = "index_point")

conserved_sub_table <- trsf_data[trsf_data$keep==1,]

# table <- conserved_sub_table
# dist_cc_TRUE <- as.numeric()
# for (i in 1:nrow(table)){
#   dist_cc_TRUE[i] = (1/1000) * geodist::geodist(x = table[i,3:4], y = table[i, 5:6])
# }
# (summary(dist_cc_TRUE))
# dist_cc_TRUE[which(dist_cc_TRUE > sqrt(2)*10)]
# (vect_indices <- table$index_point[which(dist_cc_TRUE > sqrt(2)*10)])
# 
# table0 <- specific_table[vect_indices,1:6]
# tableF <- table[which(dist_cc_TRUE > sqrt(2)*10),1:7]
# 
# intermediate_table[which(intermediate_table$index_point == vect_indices[1]),]
# table0[1,]
# tableF[1,]

### removing the "ejected" points in the sea and so on :
colnames(conserved_sub_table)

dist_cc_TRUE <- as.numeric()
for (i in 1:nrow(conserved_sub_table)){
  dist_cc_TRUE[i] = (1/1000) * geodist::geodist(x = conserved_sub_table[i,3:4], y = conserved_sub_table[i, 5:6])
}
dist_cc_TRUE
which(dist_cc_TRUE < sqrt(2)*10)
length(which(dist_cc_TRUE < sqrt(2)*10))
ultimate_conserved_sub_table <- conserved_sub_table[which(dist_cc_TRUE < sqrt(2)*10),]
ultimate_conserved_sub_table <- subset(ultimate_conserved_sub_table, select = -c(index_point,keep) )


