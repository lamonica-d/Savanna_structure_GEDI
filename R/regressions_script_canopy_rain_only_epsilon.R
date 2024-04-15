
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

vect_names = c("all_Africa","Guinean_forest-savanna","West_Sudanian_savanna","Sahelian_Acacia_savanna",
          "Northern_Congolian_Forest-Savanna","Western_Congolian_forest-savanna","Southern_Congolian_forest-savanna")

vect_names

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

# + EPSILON
table_region[,"canopy_cover"] <- table_region[,"canopy_cover"] + 10**-3

if(print_correlations == TRUE){
  
cor_matrix <- cor(table_region[,c("rh98","canopy_cover","mean_precip_std","fire_freq_std","mean_temp_std")])
cor_matrix[lower.tri(cor_matrix)] <- NA
print(round(cor_matrix,2))

}

if(launch_brms == TRUE){
  
start <- Sys.time()
print(start)
print("canopy_cover ~ mean_precip_std")

mod_canopy_cover <- brm(
                        formula = canopy_cover ~ mean_precip_std,
                        data = table_region,
                        family = brmsfamily(
                          family = "beta",
                          # in ]0,1[]
                          link = "logit",
                          link_phi = "log"
                        ),
                        prior = NULL,
                        warmup = 2*10**3,
                        iter = 10**4,
                        thin = 10,

                        chains = 3,
                        cores = 3,

                        # control = list(adapt_delta = 0.95),

                        silent = 1
                        # full comments
)

print(Sys.time() - start)

if (save_rds_files ==TRUE){
                          saveRDS(mod_canopy_cover,
                                  file.path(
                                    path_to_Savanna_structure_GEDI_folder,
                                    "outputs",
                                    "canopy_cover_rain_only_epsilon", # !!!!!!!!!
                                    paste0(name,"_regression_canopy_cover.RDS")
                                  )
)

print(paste(paste0(name,"_regression_canopy_cover.RDS"),"DONE"))
}

} # end of brms part

} # loop end

# Vérifier si le modèle est bon :
mod_canopy_cover

