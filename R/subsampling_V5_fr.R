##inference points per cell
library(brms)
library(posterior)

#data prep
data <- readRDS(file.path("transformed_data_ilots","data_point_in_cells_10e4.RDS"))
classes_prec <- c(793, 1020, 1235)
classes_prec_std <- c(0.5376, 0.9949, 1.428)

#rm cc == 0 & add info precipitation classes
unlist_data <- data.frame()
for (j in 1:length(data)){
  temp <- data[[j]]
  
  if (length(temp) != 0){
  temp2 <- subset(temp, cc != 0)
  class_prec <- ifelse(temp2$prec_std <= classes_prec_std[1],1,
                       ifelse(temp2$prec_std > classes_prec_std[3],4,
                              ifelse(temp2$prec_std > classes_prec_std[1] & temp2$prec_std <= classes_prec_std[2],2,3)
                       ))
  temp2 <- cbind(temp2, class_prec = as.factor(class_prec), index_list = j)
  
  cell_id_vect <- unique(temp2$cell_id)
  
  #on enleve les cellules ac moins de 20 pts MAIS TTES CLASSES DE PREC CONFONDUES
    for (i in 1:length(cell_id_vect)){ 
      cell_select <- temp2[which(temp2$cell_id==cell_id_vect[i]),]
      #print(nrow(cell_select))
      if (nrow(cell_select) < 20){
        temp2 <- subset(temp2, cell_id!=cell_id_vect[i])
        }else{next}
    }
  
  unlist_data <- rbind(unlist_data, temp2)
  
  }else{next}
}

unique_id <- 0
unique_id_vect <- as.numeric()
j_unique <- unique(unlist_data$index_list)
for (j in j_unique){
  temp <- unlist_data[which(unlist_data$index_list==j),]
  cell_id_vect <- unique(temp$cell_id)
  
  for (i in 1:length(cell_id_vect)){ 
    unique_id <- unique_id + 1
    cell_select <- temp[which(temp$cell_id==cell_id_vect[i]),]
    unique_id_vect <- c(unique_id_vect, rep(unique_id, nrow(cell_select)))
  }
}
unlist_data <- cbind(unlist_data, unique_id = unique_id_vect)
data_prec1 <- subset(unlist_data, class_prec == 1)
data_prec2 <- subset(unlist_data, class_prec == 2)
data_prec3 <- subset(unlist_data, class_prec == 3)
data_prec4 <- subset(unlist_data, class_prec == 4)

# for (i in unique(unlist_data$unique_id)[1000:1010]){
#   print(summary(unlist_data[unlist_data$unique_id ==i,]$class_prec))
#  }
############################################################

rm(list = c("temp","temp2","unlist_data","data","cell_select","unique_id_vect",
"cell_id_vect","class_prec","i","j","unique_id","j_unique"))

data1 <- list(data_prec1,data_prec2,data_prec3,data_prec4)
data_all <- list()
#on re enleve les cellules avec moins de 20 points
for (j in 1:4){
  temp_og <- data1[[j]]
unique_id_vect <- unique(temp_og$unique_id)
for (i in 1:length(unique_id_vect)){
  temp <- subset(temp_og, unique_id == unique_id_vect[i])
  if (nrow(temp) < 20){
    temp_og <- subset(temp_og, unique_id!=unique_id_vect[i])
  }else{next}
}
data_all[[j]] <- temp_og
}

rm(list = c("temp_og","data1","temp","unique_id_vect","i","j"))

##on ne garde que xx points per cell randomly selected
#combien ya de point per cell, prendre la mediane du nombre de points par ex
nb_point_percell_list <- list()
for (j in 1:4){
  temp_og <- data_all[[j]]
  unique_id_vect <- unique(temp_og$unique_id)
  nb_point_percell <- as.numeric()
for (i in 1:length(unique_id_vect)){
  temp <- subset(temp_og, unique_id == unique_id_vect[i])
nb_point_percell[i] <- nrow(temp)
}
  print(j)
print(summary(nb_point_percell))
nb_point_percell_list[[j]] <- nb_point_percell
}
#moy de tlm en nb de points = 370, on va prendre ça pour le nb max
max_pts <- round(mean(unlist(lapply(nb_point_percell_list, mean))))

set.seed(1234)
data_subsampled <- list()
for (j in 1:4){
  temp_new <- data.frame()
  temp_og <- data_all[[j]]
  unique_id_vect <- unique(temp_og$unique_id)
  for (i in 1:length(unique_id_vect)){
    temp <- subset(temp_og, unique_id == unique_id_vect[i])
    if (nrow(temp) > max_pts){
      temp <- temp[sample(nrow(temp), size=max_pts), ]
    }else{ }
    temp_new <- rbind(temp_new,temp)
  }
  data_subsampled[[j]] <- temp_new
}

rm(list = c("temp_og","temp_new","temp","unique_id_vect","nb_point_percell","i","j",
            "data_prec1", "data_prec2","data_prec3","data_prec4"))

for (j in 1:4){
  temp_og <- data_subsampled[[j]]
  unique_id_vect <- unique(temp_og$unique_id)
  nb_point_percell <- as.numeric()
  for (i in 1:length(unique_id_vect)){
    temp <- subset(temp_og, unique_id == unique_id_vect[i])
    nb_point_percell[i] <- nrow(temp)
  }
  print(j)
  print(summary(nb_point_percell))
}

saveRDS(data_all, file.path("transformed_data_ilots",
                                "data_point_in_cells_10e4_prec.RDS"))
