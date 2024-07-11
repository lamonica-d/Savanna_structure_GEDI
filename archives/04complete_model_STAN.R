# first analysis global model

# Cleaning the environment
rm(list=ls())
# Getting the paths
source("paths.R")
source("R/paths.R")
setwd(file.path(path_to_Savanna_structure_GEDI_folder,"R"))
# Libraries
library(fst)
library(rjags)
library(stringr) 
library(rstan)
stan_version() # [1] "2.32.2" le 21/03/24

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

# Encoding :
print("é è û")

save_rds_files = TRUE

vect_names = c("all_Africa","Guinean_forest-savanna","West_Sudanian_savanna","Sahelian_Acacia_savanna",
                            "Northern_Congolian_Forest-Savanna","Western_Congolian_forest-savanna","Southern_Congolian_forest-savanna")

# for tests :
vect_names = vect_names[3]
name = vect_names[1]

for(name in vect_names){
  
print(name)
# load data
table_region <- readRDS(
    file.path(
      path_to_Savanna_structure_GEDI_folder,
      "transformed_data",
      paste0(name,".RDS"))
  )


fire_freq_nozero <- table_region$fire_freq
sum(abs(fire_freq_nozero)<10**-10)
fire_freq_nozero[abs(fire_freq_nozero)<10**-10] <- 10**-3
# ne vaut-il pas mieux faire un test type abs()<10**-14 ?
# test d'égalité sur les flottants

canopy_cover_nozero <- table_region$canopy_cover
sum(abs(canopy_cover_nozero)<10**-10)
canopy_cover_nozero[abs(canopy_cover_nozero)<10**-10] <- 10**-3

# MCMC initialisation
init <- list(
              list(
                K_G_t_f_pluie_max = 16,
                pt_inflexion_grass = 410,
                pt_inflexion_feu = 1.5,
                
                lambda = runif(nrow(table_region),0.0017,0.0088)
              ),
              
              list(
                K_G_t_f_pluie_max = 14,
                pt_inflexion_grass = 460,
                pt_inflexion_feu = 0.5,
                
                lambda = runif(nrow(table_region),0.0017,0.0088)
              ),
              
              list(
                K_G_t_f_pluie_max = 15,
                pt_inflexion_grass = 420,
                pt_inflexion_feu = 1.2,
                
                lambda = runif(nrow(table_region),0.0017,0.0088)
              )   
  
)

# MCMC

mcmc_STAN <- stan(
  
                  file = file.path(path_to_Savanna_structure_GEDI_folder,
                                   "R",
                                   "STAN_models",
                                   "complete_model.stan"),
                  
                  data = list(
                              N = nrow(table_region), 
                              prec_data = table_region$mean_precip,
                              fire_data = fire_freq_nozero, 
                              cc_data = canopy_cover_nozero,
                              delta_min = 0.0017,
                              delta_max = 0.0088
                  ),
                  
                  init = init,
                  
                  iter = 5*10**4,
                  thin = 10,
                  warmup = 3*10**3, # burnin (not the pre-burnin phase)
                  chains = 3, # default is always 4
                  cores = 3,
                  # we recommend setting it to be as many processors
                  # as the hardware and RAM allow (up to the number of chains).
                  verbose = TRUE
                )

# Summary
print(mcmc_STAN)

# Save output
if(save_rds_files==TRUE){
  
        saveRDS(mcmc,
                file.path(
                  path_to_Savanna_structure_GEDI_folder,
                  "outputs",
                  "STAN_outputs",
                  paste0(name,"_complete_model.RDS"))
                )
        }

} # end of vect_names loop

print(summary(mcmc))


# # Verification
# list_files <- list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs","JAGS_outputs"))
# list_files
# list_files = "West_Sudanian_savanna_complete_model2.RDS"
# file = "West_Sudanian_savanna_complete_model2.RDS"
#   
# for(file in list_files){
#   
#   print(str_sub(file,end=-21))
#   
#   mcmc <- readRDS(file.path(path_to_Savanna_structure_GEDI_folder,"outputs","JAGS_outputs",file))
#   print(summary(mcmc))
#   # first checks
#   # gelman.diag(mcmc)
#   # autocorr.plot(mcmc[[1]], ask = "TRUE")
#   # plot(mcmc, trace = T)
#   # mtot <- as.data.frame(as.matrix(mcmc1))
#   
#   # quick boxplots of regression coefficients
#   # par(mfrow = c(1,1), ask = "N")
#   # boxplot(mtot[,1:9], las = 1)
#   # abline(h = 0, lty = 3, col = "red")
#   # boxplot(mtot[,10:12], las = 1)
#   
# }
# 
