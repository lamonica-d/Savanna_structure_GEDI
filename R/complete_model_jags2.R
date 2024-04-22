# first analysis global model

# Cleaning the environment
rm(list=ls())
# Getting the paths
source("paths.R")
source("R/paths.R")
# Libraries
library(fst)
library(rjags)
library(stringr) 

vect_names = c("all_Africa","Guinean_forest-savanna","West_Sudanian_savanna","Sahelian_Acacia_savanna",
                            "Northern_Congolian_Forest-Savanna","Western_Congolian_forest-savanna","Southern_Congolian_forest-savanna")

# for tests :
vect_names = vect_names[2]
name = vect_names[1]

save_rds_files = TRUE

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
  
  # table_region <- cbind(table_region, fire_freq_nozero = fire_freq_nozero,
  #                       canopy_cover_nozero = canopy_cover_nozero)
  head(table_region,2)
  
  data <- list(
               N = nrow(table_region), 
               prec_data = table_region$mean_precip,
               fire_data = fire_freq_nozero, 
               cc_data = canopy_cover_nozero,
               delta_min = 0.0017,
               delta_max = 0.0088
               ) 
  
  # model and jags inference
  model <-  "
  model {
    
    for (i in 1:N){
      
      sigmo_pluie[i] = (1/(1+exp(-lambda*(prec_data[i]-pt_inflexion_grass))))
      
      canopy_influence[i] = delta_max + (1-cc_data[i])*delta_min
      sigmo_competition[i] = (1/(1+exp(canopy_influence[i]*(prec_data[i]-pt_inflexion_grass))))
      
      grassB[i] = K_G_t_f_pluie_max*sigmo_pluie[i]*sigmo_competition[i]
  }
    
  for (i in 1:N){
      
      fire_data[i] ~ dnorm( min ( max(0.001, (grassB[i]**2)/(grassB[i]**2 + pt_inflexion_feu**2) ) , 0.999) , 1/0.01)
      
      }
  
  # priors
  
  pt_inflexion_grass ~ dnorm(410,1/50)
  pt_inflexion_feu ~ dnorm(1.5,1/1)
  K_G_t_f_pluie_max ~ dnorm(16,1/5)
  lambda ~ dunif(0.0017,0.0088)
  
  }
  "
  
  inits <- list(
                 list(
                      K_G_t_f_pluie_max = 16,
                      pt_inflexion_grass = 410,
                      pt_inflexion_feu = 1.5,
                        
                      lambda = runif(1,0.0017,0.0088)
                      ),
                 
                 list(
                   K_G_t_f_pluie_max = 14,
                   pt_inflexion_grass = 460,
                   pt_inflexion_feu = 0.5,
                   
                   lambda = runif(1,0.0017,0.0088)
                 ),
                 
                 list(
                   K_G_t_f_pluie_max = 15,
                   pt_inflexion_grass = 420,
                   pt_inflexion_feu = 1.2,
                   
                   lambda = runif(1,0.0017,0.0088)
                 )   
                 
                )
  
  m <- rjags::jags.model(
                          textConnection(model),
                          inits = inits, 
                          data = data, 
                          n.chains = length(inits)
                          )
  
  update(m, 1000)
  mcmc <- rjags::coda.samples(
                               m,
                               variable.names = c("K_G_t_f_pluie_max",
                                                  "lambda",
                                                  "pt_inflexion_feu",
                                                  "pt_inflexion_grass"
                                                  ),
                               n.iter = 5000,
                               thin = 5
                               ) 
  
  if(save_rds_files==TRUE){
    
          saveRDS(mcmc,
                  file.path(
                    path_to_Savanna_structure_GEDI_folder,
                    "outputs",
                    "JAGS_outputs",
                    paste0(name,"_complete_model2.RDS"))
                  )
          }

} # end of vect_names loop

print(summary(mcmc))
summary(mcmc)$statistics
# For R/simulations_fonction_grass/simu_sigmo_post_mcmc.Rmd :
{
  K_G_t_f_pluie_max_mcmc = summary(mcmc)$statistics["K_G_t_f_pluie_max",1]
  pt_inflexion_grass_mcmc = summary(mcmc)$statistics["pt_inflexion_grass",1]
  lambda_mcmc = summary(mcmc)$statistics["lambda",1]
  pt_inflexion_feu_mcmc = summary(mcmc)$statistics["pt_inflexion_feu",1]
}

autocorr.plot(mcmc[[1]], ask = "TRUE")
plot(mcmc, trace = T)



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