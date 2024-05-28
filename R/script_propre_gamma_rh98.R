
# Cleaning the environment
rm(list=ls())
# Getting the paths
source("paths.R")
source("R/paths.R")
path_to_R_folder = file.path(path_to_Savanna_structure_GEDI_folder,"R")
setwd(path_to_R_folder)

# Libraries
library(fst)
library(rjags)
library(rstan)
stan_version()
library(stringr) 
library(brms)
library(corrplot)

# Rstan commands :
options(mc.cores = parallel::detectCores())
# if you are using rstan locally on a multicore machine and have plenty of RAM
# to estimate your model in parallel
rstan_options(auto_write = TRUE)
# which allows you to automatically save a bare version of a compiled Stan program
# to the hard disk so that it does not need to be recompiled (unless you change it).
# You will need to run these commands each time you load the rstan library.

# To verify rstan installation :
# example(stan_model, package = "rstan", run.dontrun = TRUE)

#################################################################

vect_names = c("Guinean_forest-savanna","West_Sudanian_savanna","Sahelian_Acacia_savanna",
          "Northern_Congolian_Forest-Savanna","Western_Congolian_forest-savanna","Southern_Congolian_forest-savanna")

# Vérifier si on a normalisé sur toutes les données ou juste sur l'ensemble de certaines régions.

# vect_names = unique(complete_table$ecoregion) # for all ecoregions
vect_names
# if 6_ecoregions_without_duplicate_standardized_ONLY_over_6_ecoregions.RDS was loaded,
# there are just the 6 ecoregions anyway

# vect_names = c("Guinean_forest-savanna") # for testing the loop

# (vect_names <- str_sub(list.files(path = path_to_GEDI_raw_data),end = -5))

save_rds_files = TRUE
plotland = FALSE
print_correlations = FALSE
launch_brms = TRUE

############ LOOP

for (name in vect_names){
print("###########################################################################")
print(name)

table_region <- readRDS(
                        file.path(
                          path_to_Savanna_structure_GEDI_folder,
                          "transformed_data",
                          paste0(name,".RDS"))
                        )

print("nrow(table_region)")
print(nrow(table_region))

if(launch_brms == TRUE){
  
start <- Sys.time()
print(start)
print("rh98 ~ fire_freq_std + mean_precip_std")

mod_rh98 <- brm(
                formula = rh98 ~ fire_freq_std + mean_precip_std,
                data = table_region,
                family = brmsfamily(family = "Gamma",link="log"),
                
                prior = NULL,
                
                warmup = 2*10**3,
                iter = 10**4,
                thin = 10,
                
                chains = 4,
                cores = 4,          
                
                # control = list(adapt_delta = 0.95), 
                
                silent = 0 # no comments # =1 full comments
)

print(Sys.time() - start)
    
print(summary(mod_rh98))

if(save_rds_files==TRUE){

  saveRDS(mod_rh98,
          file.path(
            path_to_Savanna_structure_GEDI_folder,
            "outputs",
            "rh98",
            paste0(name,"_regression_rh98.RDS"))
          )

  print(paste(paste0(name,"_regression_rh98.RDS"),"DONE"))
}

} # end of brms part

} # loop end

########### glm() function

for (name in vect_names){
  print("###########################################################################")
  print(name)
  
  table_region <- readRDS(
    file.path(
      path_to_Savanna_structure_GEDI_folder,
      "transformed_data",
      paste0(name,".RDS"))
  )
  
  res <- glm(formula = rh98 ~ fire_freq_std + mean_precip_std,
      data = table_region,
      family = Gamma(link = "log"))
  
  print(summary(res))
}
