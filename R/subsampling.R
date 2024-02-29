#subsampling of ecoregion tables
rm(list=ls())
# Getting the paths
source("R/paths.R")
# Libraries
library(fst)
library(brms)

vect_names <- list.files(path = path_to_GEDI_raw_data)

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
#save(df_for_std, file="outputs/values_for_covariables_standardisation")
############
load(file="outputs/values_for_covariables_standardisation")

#load and transform data for the three selected ecoregions
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


