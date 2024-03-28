# subsampling of ecoregion tables
rm(list=ls())
# Getting the paths
source("paths.R")
# Libraries
library(fst)
library(brms)
library(terra)

getwd()
(vect_names <- list.files(path = path_to_GEDI_raw_data))

#####compute mean and sd of mean_temp, mean_prec, fire_frq for all data####
temp_vect <- as.numeric()
prec_vect <- as.numeric()
fire_vect <- as.numeric()

for (i in 1:length(names)){
  
  table <- fst::read.fst(paste0(path_to_GEDI_raw_data,"/",vect_names[i],sep=""))
  table[is.na(table[,"fire_freq"]),"fire_freq"] <- 0
  table <- table[complete.cases(table[,c("mean_precip","mean_temp","fire_freq")]),]
  
  temp_vect <- c(temp_vect, table$mean_temp)
  prec_vect <- c(prec_vect, table$mean_precip)
  fire_vect <- c(fire_vect, table$fire_freq)
}
rm(table)
mean_temp <- mean(temp_vect, na.rm = T)
mean_prec <- mean(prec_vect, na.rm = T)
mean_fire <- mean(fire_vect, na.rm = T)
sd_temp <- sd(temp_vect, na.rm = T)
sd_prec <- sd(prec_vect, na.rm = T)
sd_fire <- sd(fire_vect, na.rm = T)

df_for_std <- data.frame(precip = c(mean_prec, sd_prec), 
                         temp = c(mean_temp, sd_temp),
                         fire = c(mean_fire, sd_fire))
# save(df_for_std, file="outputs/values_for_covariables_standardisation")
############
load(file="outputs/values_for_covariables_standardisation")

### grid resampling
#0) load data
table <- fst::read.fst(paste0(path_to_GEDI_raw_data,"/",vect_names[20], sep=""))
# To replace the NA by zeros :
table[is.na(table[,"fire_freq"]),"fire_freq"] <- 0
table <- table[complete.cases(table[,c("mean_precip","mean_temp","fire_freq")]),]
x <- table
rm(table)

########################################## x <- la_table_propre_de_l_autre_fichier_la

x <- cbind(x$x,x$y,x)
colnames(x)
ncol(x)

colnames(x)[1] = "x_TRUE"
colnames(x)[2] = "y_TRUE"

colnames(x)

#1) setting resolution
#from data.frame to spatvector
x <- terra::vect(x, geom = c("x", "y"), crs = "+proj=longlat +datum=WGS84") 
# get x "window" : xmin, xmax, ymin, ymax
x_ext <- terra::ext(x)
# set a cell length, in meter
cell <- 10000
dx <- geodist::geodist(x = c(x_ext[1],x_ext[3]), y = c(x_ext[2],x_ext[3]), measure = "haversine")
dy <- geodist::geodist(x = c(x_ext[1],x_ext[3]), y = c(x_ext[1],x_ext[4]), measure = "haversine")
(target_ncol <- round(dx/cell))
(target_nrow <- round(dy/cell))

#2) resampling
y <- terra::rast(x, ncol = target_ncol, nrow = target_nrow)
z <- terra::rasterize(x, y, fun = sample, size = 1, field = colnames(terra::values(x)))
# fun = sample , size = 1 to get one random sample in the cell
trsf_data <- data.frame(cbind(crds(y), values(z)))

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

# # Tests of odd/even cases :
# conserved <- grid_of_distant_cells(3,5,TRUE)
# conserved <- grid_of_distant_cells(4,5,TRUE)
# conserved <- grid_of_distant_cells(3,6,TRUE)
# conserved <- grid_of_distant_cells(4,6,TRUE)

conserved <- grid_of_distant_cells(target_nrow,target_ncol)

length(conserved)
summary(conserved)
mean(conserved)

conserved_indices = rep(NA,length(conserved))

# deletions
for (s in 1:length(conserved)){
  if(conserved[s] == TRUE){conserved_indices[s] = s}
}

conserved_indices
conserved_indices <- conserved_indices[!is.na(conserved_indices)]
conserved_indices
sum(is.na(conserved_indices))

summary(conserved)
length(conserved_indices)

summary(conserved_indices)
nrow(trsf_data)

conserved_sub_table <- trsf_data[conserved_indices,]
nrow(conserved_sub_table)

colnames(conserved_sub_table)
ncol(conserved_sub_table)

colnames(conserved_sub_table)[1] = "x_center_cell"
colnames(conserved_sub_table)[2] = "y_center_cell"

colnames(conserved_sub_table)

# Normalement les données ne comportant déjà plus de NA, donc ligne suivante inutile
conserved_sub_table <- conserved_sub_table[
                        complete.cases(conserved_sub_table[,c("x_center_cell",
                                                              "y_center_cell",
                                                              "x_TRUE",
                                                              "y_TRUE",
                                                              "rh98",
                                                              "canopy_cover",
                                                              "fire_freq",
                                                              "mean_precip",
                                                              "mean_temp",
                                                              "ecoregion"
                                                              # "fire_freq_NA"
                                                               )
                                                           ]
                                        ),]

nrow(conserved_sub_table)

# Pour enregistrer les 850 points en .geojson
require(sf)

sf_obj <- st_as_sf(conserved_sub_table,coords = c("x_center_cell", "y_center_cell"),crs = 4326)

st_write(sf_obj,
         file.path(path_to_Savanna_structure_GEDI_folder,
                   "geojson_files",
                   "center_cells.geojson"
                   )
         )

sf_obj2 <- st_as_sf(conserved_sub_table,coords = c("x_TRUE", "y_TRUE"),crs = 4326)

st_write(sf_obj2,
         file.path(path_to_Savanna_structure_GEDI_folder,
                   "geojson_files",
                   "TRUE_positions.geojson"
         )
)

########################################## 

#3) standardisation
sub_table$fire_freq <- (sub_table$fire_freq-df_for_std$fire[1])/df_for_std$fire[2]
sub_table$mean_precip <- (sub_table$mean_precip-df_for_std$precip[1])/df_for_std$precip[2]
sub_table$mean_temp <- (sub_table$mean_temp -df_for_std$temp[1])/df_for_std$temp[2]

######## JUST RANDOM SAMPLING##############
# load and transform data for the three selected ecoregions
(vect_names_3 <- vect_names[c(10,17,20)])
sub_table_std_list <- list()
for (i in 1:3){
table <- fst::read.fst(paste0(path_to_GEDI_raw_data,"/",vect_names_3[i], sep=""))
# To replace the NA by zeros :
table[is.na(table[,"fire_freq"]),"fire_freq"] <- 0
table <- table[complete.cases(table[,c("mean_precip","mean_temp","fire_freq")]),]

set.seed(1234)
sub_table <- table[sample(1:nrow(table),2*10**4, replace = F),]
rm(table)
#standardisation
sub_table$fire_freq <- (sub_table$fire_freq-df_for_std$fire[1])/df_for_std$fire[2]
sub_table$mean_precip <- (sub_table$mean_precip-df_for_std$precip[1])/df_for_std$precip[2]
sub_table$mean_temp <- (sub_table$mean_temp -df_for_std$temp[1])/df_for_std$temp[2]

sub_table_std_list[[i]] <- sub_table
rm(sub_table)
}
sub_Guinean_table <- sub_table_std_list[[1]]
sub_Sahelian_table <- sub_table_std_list[[2]]
sub_WestSudanian_table <- sub_table_std_list[[3]]
#############################################

####VARIOGRAMMES
#coords in lat/long, should they be in meters ?
table <- fst::read.fst(paste0(path_to_GEDI_raw_data,"/",vect_names[7], sep=""))
table[is.na(table[,"fire_freq"]),"fire_freq"] <- 0
table <- table[complete.cases(table[,c("mean_precip","mean_temp","fire_freq")]),]
meter_coord <- proj4::project(xy = table[,1:2], proj = "+proj=utm +datum=WGS84")
meter_coord <- data.frame(x = meter_coord$x, y = meter_coord$y)
var_rh98 <- geoR::variog(coords = meter_coord[(1:(9*10^4)),],
                         data = table$rh98[(1:(9*10^4))])

####### tree height (rh98) #############
default_prior = get_prior(
  formula = rh98 ~ mean_precip + mean_temp  + fire_freq,
  data = sub_WestSudanian_table,
  family = brmsfamily(family = "Gamma")
)

start <- Sys.time()
print(start)

mod <- brm(
  
  formula = rh98 ~ mean_precip + mean_temp+ fire_freq,
  data = sub_Guinean_table,
  
  family = brmsfamily(family = "Gamma"),
  prior = NULL,
  
  warmup = 2*10**3,
  iter = 6*10**3,
  thin = 10,
  
  file = "outputs/test_brms_Sahelian_rh98_dom1.RDS",
  
  chains = 3,
  cores = 3,    
  silent = 0
)

print(Sys.time() - start)
plot(mod)

####### canopy cover #############
default_prior = get_prior(
  formula = canopy_cover ~ mean_precip + mean_temp + fire_freq,
  data = sub_WestSudanian_table,
  family = brmsfamily(family = "zero_one_inflated_beta")
)

start <- Sys.time()
print(start)

mod <- brm(
  
  formula = canopy_cover ~ mean_precip + mean_temp  + fire_freq,
  data = sub_WestSudanian_table,
  
  family = brmsfamily(family = "zero_one_inflated_beta"),
  prior = NULL,
  
  warmup = 2*10**3,
  iter = 6*10**3,
  thin = 10,
  
  file = "outputs/test_brms_WSudanian_canopycover_dom1.RDS",
  
  chains = 3,
  cores = 3,    
  silent = 0
)

print(Sys.time() - start)

plot(mod)

##################################


