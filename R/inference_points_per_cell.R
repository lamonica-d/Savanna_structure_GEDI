##inference points per cell

data <- readRDS(file.path("transformed_data_ilots","data_point_in_cells_10e4.RDS"))

for (j in 1:length(data)){
 for (i in 1:max(data[[j]]$cell_id)){
   temp <- data[[j]]
   print(length(which(temp$cell_id==i)))
 }
}