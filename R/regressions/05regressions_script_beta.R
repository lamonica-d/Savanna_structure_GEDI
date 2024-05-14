
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

if(print_correlations == TRUE){
  
cor_matrix <- cor(table_region[,c("rh98","canopy_cover","mean_precip_std","fire_freq_std","mean_temp_std")])
cor_matrix[lower.tri(cor_matrix)] <- NA
print(round(cor_matrix,2))

}

if(plotland == TRUE){
  
  # Quick summaries
  
  print(summary(table_region[,c("rh98","canopy_cover")]))
  print("-_____-")
  print(summary(table_region[,c("fire_freq","mean_precip","mean_temp")]))
  print("-_____________________-")
  print(summary(table_region[,c("fire_freq_std","mean_precip_std","mean_temp_std")]))
  
  # see Savanna_structure_GEDI/figures/hists_canopy_rain_fire
  # for histograms of rh98 and canopy_cover
  
plot(table_region[,c("canopy_cover","rh98")],
     main=round(cor(table_region[,c("canopy_cover","rh98")]),2))

plot(table_region[,c("mean_precip_std","rh98")],
     main=round(cor(table_region[,c("mean_precip_std","rh98")]),2))
plot(table_region[,c("fire_freq_std","rh98")],
     main=round(cor(table_region[,c("fire_freq_std","rh98")]),2))
plot(table_region[,c("mean_temp_std","rh98")],
     main=round(cor(table_region[,c("mean_temp_std","rh98")]),2))


plot(table_region[,c("mean_precip_std","canopy_cover")],
     main=round(cor(table_region[,c("mean_precip_std","canopy_cover")]),2))
plot(table_region[,c("fire_freq_std","canopy_cover")],
     main=round(cor(table_region[,c("fire_freq_std","canopy_cover")]),2))
plot(table_region[,c("mean_temp_std","canopy_cover")],
     main=round(cor(table_region[,c("mean_temp_std","canopy_cover")]),2))

}

############################################# canopy_cover

# default_prior = get_prior(
#                           formula = canopy_cover ~ mean_precip + fire_freq,
#                           
#                           data = table_region,
#                           
#                           family = brmsfamily(
#                             family = "zero_inflated_beta",
#                             link = "logit",
#                             link_phi = "log",
#                             link_zi = "logit"
#                           )
#                           )
# 
# 
# prior_2 = c(
#             prior(
#               normal(1400,100),
#               class="b",
#               coef = mean_precip
#             ),
#             
#             prior(
#               normal(26,4),
#               class="b",
#               coef = mean_temp
#             ),
#             
#             prior(
#               normal(0.25,0.5),
#               class="b",
#               coef = fire_freq
#             )
# )


if (launch_brms==TRUE){
start <- Sys.time()
print(start)
print("canopy_cover ~ fire_freq_std + mean_precip_std")

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

                        silent = 1
                        # full comments
)

print(Sys.time() - start)

if (save_rds_files ==TRUE){
                          saveRDS(mod_canopy_cover,
                                  file.path(
                                    path_to_Savanna_structure_GEDI_folder,
                                    "outputs",
                                    "canopy_cover",
                                    paste0(name,"_regression_canopy_cover.RDS")
                                  )
)

print(paste(paste0(name,"_regression_canopy_cover.RDS"),"DONE"))
}

} # end of brms part

} # loop end

