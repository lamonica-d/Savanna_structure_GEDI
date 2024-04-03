
# Cleaning the environment
rm(list=ls())
# Getting the paths
source("paths.R")
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

# (vect_names <- str_sub(list.files(path = path_to_GEDI_raw_data),end = -5))
vect_names = c("Guinean_forest-savanna","West_Sudanian","Sahelian_Acacia")
# name = "Guinean_forest-savanna"

save_rds_files = FALSE
plotland = FALSE
print_correlations = TRUE

############ LOOP

for (name in vect_names){

print(name)

table_region <- readRDS(
                        file.path(
                          path_to_Savanna_structure_GEDI_folder,
                          "transformed_data",
                          paste0(name,".RDS"))
                        )

table_region = subset(table_region, select = -c(index_point,keep) )

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

############################################# rh98

###### Priors

# default_prior = get_prior(
#   formula = rh98 ~ fire_freq_std + mean_precip_std,
#   data = table_region,
#   family = brmsfamily(family = "Gamma")
#   # no info about the links in (*)
#   # unlike the beta inflated
# )

# prior_1 = c(
#   prior(
#     normal(1400,100),
#     class="b",
#     coef = mean_precip
#   ),
#   
#   prior(
#     normal(26,4),
#     class="b",
#     coef = mean_temp
#   ),
#   
#   prior(
#     normal(0.25,0.5),
#     class="b",
#     coef = fire_freq
#   )
# )

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
                
                # to save/load the file automatically
                file = file.path(
                                path_to_Savanna_structure_GEDI_folder,
                                "outputs",
                                "rh98",
                                paste0(name,"_regression_rh98.RDS")
                                ),
                
                chains = 4,
                cores = 4,          
                
                # control = list(adapt_delta = 0.95), 
                
                silent = 1 # full comments
)

print(Sys.time() - start)

# if(save_rds_files==TRUE){
#   
#   saveRDS(mod_rh98,
#           file.path(
#             path_to_Savanna_structure_GEDI_folder,
#             "outputs",
#             "rh98",
#             paste0(name,"_regression_rh98.RDS"))
#           )
#   
#   print(paste(paste0(name,"_regression_rh98.RDS"),"DONE"))
# }


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


start <- Sys.time()
print(start)
print("canopy_cover ~ fire_freq_std + mean_precip_std")

mod_canopy_cover <- brm(
                        formula = canopy_cover ~ fire_freq_std + mean_precip_std,
                        data = table_region,
                        family = brmsfamily(
                          family = "zero_inflated_beta",
                          link = "logit",
                          link_phi = "log",
                          link_zi = "logit"
                        ),
                        prior = NULL,
                        warmup = 2*10**3,
                        iter = 10**4,
                        thin = 10,
                        
                        # to save/load the file automatically
                        file = file.path(
                                         path_to_Savanna_structure_GEDI_folder,
                                         "outputs",
                                         "canopy_cover",
                                         paste0(name,"_regression_canopy_cover.RDS")
                        ),
                        
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

} # loop end


########################### RESULT ANALYSIS

#################################### Cleaning the environment if needed
rm(list=ls())
# Getting the paths
source("paths.R")
path_to_R_folder = file.path(path_to_Savanna_structure_GEDI_folder,"R")
setwd(path_to_R_folder)
getwd()
#######################################################################

require(shinystan)
lancer_shinystan = FALSE

### rh98
list_of_ouputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs","rh98"),full.names=TRUE)
names_of_outputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs","rh98"),full.names=FALSE)

print("..･ヾ(。￣□￣)ﾂ")
for(i in 1:length(list_of_ouputs)){
  print(names_of_outputs[i])
  print("########################################")
  
  mcmc_output <- readRDS(list_of_ouputs[i])
  print(summary(mcmc_output))
  if(lancer_shinystan == TRUE ){launch_shinystan(mcmc_output)}
  print("########################################")
}

print(mcmc_output)
# plot(mcmc_output,ask=FALSE)
stancode(mcmc_output)

### canopy_cover
list_of_ouputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs","rh98"),full.names=TRUE)
names_of_outputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs","rh98"),full.names=FALSE)

print("..･ヾ(。￣□￣)ﾂ")
for(i in 1:length(list_of_ouputs)){
  print(names_of_outputs[i])
  print("########################################")
  
  mcmc_output <- readRDS(list_of_ouputs[i])
  print(summary(mcmc_output))
  if(lancer_shinystan == TRUE ){launch_shinystan(mcmc_output)}
  print("########################################")
}

print(mcmc_output)
# plot(mcmc_output,ask=FALSE)
stancode(mcmc_output)

########################################### Predictive density


list_of_ouputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs"),full.names=TRUE)
names_of_outputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs"),full.names=FALSE)

# test Gamma rh98 Guinean

print(names_of_outputs[2]) 
mcmc_output <- readRDS(list_of_ouputs[2])
summary(mcmc_output)
ls(summary(mcmc_output))

summary(mcmc_output)$fixed
summary(mcmc_output)$fixed$Estimate

ls(summary(mcmc_output))
summary(mcmc_output)$spec_pars
summary(mcmc_output)$spec_pars$Estimate

beta = as.matrix(summary(mcmc_output)$fixed$Estimate)
table_region <- readRDS(
                        file.path(
                          path_to_Savanna_structure_GEDI_folder,
                          "transformed_data",
                          paste0("Guinean_forest-savanna",".RDS"))
                        )

table_region_rows = as.matrix(
                             cbind(rep(1,nrow(table_region)),
                                subset(table_region, select = c(fire_freq_std,mean_precip_std))
                                  )
                             )
                         
linear_predictor = table_region_rows%*%beta
dim(linear_predictor)

brms_theta = exp(linear_predictor)
brms_shape = summary(mcmc_output)$spec_pars$Estimate

# c(1,1)/c(2,3)
R_scale = brms_theta/brms_shape
R_shape = brms_shape

simu_gamma
for(i in 1:n=nrow(linear_predictor)){
  
          simu_gamma <- rgamma(
            n=1000,
            shape = R_shape, 
            scale = R_scale[i]
          )
}

plot(simu_gamma)
hist(simu_gamma,breaks=30,freq=FALSE)

hist(simu_gamma,breaks=50,freq=FALSE)
hist(table_region$rh98,breaks=50,freq=FALSE)
hist(table_region$canopy_cover,breaks=50,freq=FALSE)
hist(table_region$mean_precip,breaks=50,freq=FALSE)

{
plot(table_region$rh98,simu_gamma)
abline(0,1,col="red")
}
##### LOOP for all rh98 predictive densities :

print("..･ヾ(。￣□￣)ﾂ")
for(i in 1:length(list_of_ouputs)){
  print(names_of_outputs[i])
  print("########################################")
  
  mcmc_output <- readRDS(list_of_ouputs[i])
  print(summary(mcmc_output))
  if(lancer_shinystan == TRUE ){launch_shinystan(mcmc_output)}
  print("########################################")
}

print(names_of_outputs[2])


