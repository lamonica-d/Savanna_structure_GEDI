
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

vect_names

save_rds_files = TRUE

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
# 
# print("nrow(table_region)")
# print(nrow(table_region))

start <- Sys.time()
print(start)
print(name)
print("canopy_cover ~ fire_freq_std + mean_precip_std ; (inflated)")

mod_canopy_cover <- brm(
                        formula = canopy_cover ~ fire_freq_std + mean_precip_std,
                        data = table_region,
                        family = brmsfamily(
                          family = "zero_inflated_beta",
                          # if in [0,1] with 1 included, we would need the zero_one_inflated_beta
                          link = "logit",
                          link_phi = "log",
                          link_zi = "logit"
                        ),
                        prior = NULL,
                        warmup = 2*10**3,
                        iter = 10**4,
                        thin = 10,

                        chains = 3,
                        cores = 3,

                        # control = list(adapt_delta = 0.95),

                        silent = 0
                        # 1 for full comments
)

print(summary(mod_canopy_cover))

if (save_rds_files ==TRUE){
  saveRDS(mod_canopy_cover,
          file.path(
            path_to_Savanna_structure_GEDI_folder,
            "outputs",
            "canopy_cover",
            paste0(name,"_regression_canopy_cover_inflated.RDS")
          )
  )
}
print(name)
###############################################################
table_region$canopy_cover <- table_region$canopy_cover + 10**-3
print("canopy_cover ~ fire_freq_std + mean_precip_std ; (not inflated)")
mod_canopy_cover <- brm(
                        formula = canopy_cover ~ fire_freq_std + mean_precip_std,
                        data = table_region,
                        family = brmsfamily(
                          family = "Beta",
                          # if in [0,1] with 1 included, we would need the zero_one_inflated_beta
                          link = "logit",
                          link_phi = "log",
                        ),
                        prior = NULL,
                        warmup = 2*10**3,
                        iter = 10**4,
                        thin = 10,
                        
                        chains = 3,
                        cores = 3,
                        
                        # control = list(adapt_delta = 0.95),
                        
                        silent = 2
                        # 1 for more comments
                        )

print(summary(mod_canopy_cover))
print(Sys.time() - start)

if (save_rds_files ==TRUE){
                          saveRDS(mod_canopy_cover,
                                  file.path(
                                    path_to_Savanna_structure_GEDI_folder,
                                    "outputs",
                                    "canopy_cover",
                                    paste0(name,"_regression_canopy_cover-plus_10_moins3.RDS")
                                  )
)

print(paste(paste0(name,"_regression_canopy_cover.RDS"),"DONE"))
}

} # loop end

##################### Test pour savoir si zi correspond juste à la proportion de zero :

# zi = 0.68 en sortie de glm

table_region <- readRDS(
  file.path(
    path_to_Savanna_structure_GEDI_folder,
    "transformed_data",
    paste0("Sahelian_Acacia_savanna",".RDS"))
)

summary(table_region)
table(table_region$canopy_cover)[1:5]
3796 / nrow(table_region)

# donc en fait il vire les zeros et il fait la regression sur le reste
