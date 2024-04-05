
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

########################################### Predictive density

# test Gamma rh98 Guinean

variable_name = "rh98"

list_of_ouputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs",variable_name),full.names=TRUE)
names_of_outputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs",variable_name),full.names=FALSE)

print(names_of_outputs[1])
mcmc_output <- readRDS(list_of_ouputs[1])
mcmc_values <- as.data.frame(mcmc_output)
J = nrow(mcmc_values)
head(mcmc_values)

mcmc_values[1,c("b_Intercept","b_fire_freq_std","b_mean_precip_std")]

table_region <- readRDS(
                        file.path(
                          path_to_Savanna_structure_GEDI_folder,
                          "transformed_data",
                          paste0("Guinean_forest-savanna",".RDS"))
                        )

make_stancode(formula = rh98 ~ fire_freq_std + mean_precip_std,
              data = table_region,
              family = brmsfamily(family = "Gamma",link="log"))

table_region_rows = as.matrix(
                             cbind(rep(1,nrow(table_region)),
                                subset(table_region, select = c(fire_freq_std,mean_precip_std))
                                  )
                            )
dim(table_region_rows)
I = nrow(table_region_rows)
# linear_predictors_for_one_beta_draw_per_column <- table_region_rows%*%t(mcmc_values[1,c("b_Intercept","b_fire_freq_std","b_mean_precip_std")])
# head(linear_predictors_for_one_beta_draw_per_column)

linear_predictors_for_one_beta_draw_per_column = c()
for(iter in 1:J){
  linear_predictors_for_one_beta_draw_per_column <- cbind(linear_predictors_for_one_beta_draw_per_column,
                                                          table_region_rows%*%t(mcmc_values[iter,c("b_Intercept","b_fire_freq_std","b_mean_precip_std")]))
}                     
dim(linear_predictors_for_one_beta_draw_per_column)
# nb_donnes I * nb_iter_mcmc J

simulations = matrix(nrow=nrow(linear_predictors_for_one_beta_draw_per_column),
                      ncol=ncol(linear_predictors_for_one_beta_draw_per_column))

# for all i,j we make a gamma draw
for(i in 1:I){
  print(paste("i",i,"/",I))
  for(j in 1:J){
    brms_theta = exp(linear_predictors_for_one_beta_draw_per_column[i,j])
    brms_shape = mcmc_values[j,"shape"]
    
    R_scale = brms_theta/brms_shape
    R_shape = brms_shape

    simulations[i,j] <- rgamma(n=1,shape = R_shape, scale = R_scale)
  }
}

hist(simulations[1,])
quantile(simulations[1,], probs = c(0.05,0.5,0.95))

resultats_finaux = matrix(nrow=I,ncol=3)
colnames(resultats_finaux) = c("quantile_inf","mean","quantile_sup")

for(i in 1:I){
  resultats_finaux[i,] <- quantile(simulations[i,], probs = c(0.05,0.5,0.95))
}

{
  plot(1:I,
       table_region$rh98,
       xlab="i",
       ylab="rh98"
  )
  lines(1:I,resultats_finaux[,2],col="pink",lty=1)
  lines(1:I,resultats_finaux[,1],col="blue",lty=2)
  lines(1:I,resultats_finaux[,3],col="darkblue",lty=2)
}

hist(table_region$mean_precip,breaks=30)
indices_of_increasing_mean_precip <- sort(table_region$mean_precip, index.return=TRUE)$ix

plot(1:length(table_region$mean_precip),
     table_region$mean_precip[indices_of_increasing_mean_precip],
     xlab="indices ordonnés",
     ylab="mean_precip croissant")

resultats_finaux <- resultats_finaux[indices_of_increasing_mean_precip,]

{
plot(1:I,
     table_region$rh98[indices_of_increasing_mean_precip],
     xlab="mean_precip croissant",
     ylab="rh98"
     )
lines(1:I,resultats_finaux[,2],col="pink",lty=1)
lines(1:I,resultats_finaux[,1],col="blue",lty=2)
lines(1:I,resultats_finaux[,3],col="darkblue",lty=2)
}

# test beta canopy_cover Guinean

variable_name = "canopy_cover"

list_of_ouputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs",variable_name),full.names=TRUE)
names_of_outputs = list.files(path = file.path(path_to_Savanna_structure_GEDI_folder,"outputs",variable_name),full.names=FALSE)

print(names_of_outputs[1])
mcmc_output <- readRDS(list_of_ouputs[1])
mcmc_values <- as.data.frame(mcmc_output)
J = nrow(mcmc_values)
head(mcmc_values)

mcmc_values[1,c("b_Intercept","b_fire_freq_std","b_mean_precip_std")]

table_region <- readRDS(
  file.path(
    path_to_Savanna_structure_GEDI_folder,
    "transformed_data",
    paste0("Guinean_forest-savanna",".RDS"))
)

make_stancode(formula = canopy_cover ~ fire_freq_std + mean_precip_std,
              data = table_region,
              family = brmsfamily(
                family = "zero_inflated_beta",
                link = "logit",
                link_phi = "log",
                link_zi = "logit")
              )
require(gtools) # for logit function

table_region_rows = as.matrix(
  cbind(rep(1,nrow(table_region)),
        subset(table_region, select = c(fire_freq_std,mean_precip_std))
  )
)
dim(table_region_rows)
I = nrow(table_region_rows)
# linear_predictors_for_one_beta_draw_per_column <- table_region_rows%*%t(mcmc_values[1,c("b_Intercept","b_fire_freq_std","b_mean_precip_std")])
# head(linear_predictors_for_one_beta_draw_per_column)

linear_predictors_for_one_beta_draw_per_column = c()
for(iter in 1:J){
  print(paste("iter",iter,"/",J))
  linear_predictors_for_one_beta_draw_per_column <- cbind(linear_predictors_for_one_beta_draw_per_column,
                                                          table_region_rows%*%t(mcmc_values[iter,c("b_Intercept","b_fire_freq_std","b_mean_precip_std")]))
}                     
dim(linear_predictors_for_one_beta_draw_per_column)
# nb_donnes I * nb_iter_mcmc J

simulations = matrix(nrow=nrow(linear_predictors_for_one_beta_draw_per_column),
                     ncol=ncol(linear_predictors_for_one_beta_draw_per_column))

# browseURL("https://distribution-explorer.github.io/continuous/beta.html")

# for all i,j we make a gamma draw
for(i in 1:I){
  print(paste("i",i,"/",I))
  for(j in 1:J){
    brms_theta = inv.logit(linear_predictors_for_one_beta_draw_per_column[i,j])
    brms_phi = mcmc_values[j,"phi"]
    brms_zi = mcmc_values[j,"zi"]
    
    R_shape1 = brms_theta*brms_phi
    R_shape2 = (1-brms_theta)*brms_phi
    
    value <- rbeta(n=1, shape1 = R_shape1, shape2 = R_shape2)
    prob_zero = brms_zi+(1-brms_zi)*value
    
    if(rbinom(n=1,size=1,prob=prob_zero)==TRUE){
      # print(paste("prob_zero == TRUE",prob_zero))
      simulations[i,j] <- prob_zero
    }else{
      # print(paste("prob_zero == FALSE",prob_zero))
      simulations[i,j] <- (1-brms_zi)*value
    }
    
  }
}

hist(simulations[1,])
quantile(simulations[1,], probs = c(0.05,0.5,0.95))

resultats_finaux = matrix(nrow=I,ncol=3)
colnames(resultats_finaux) = c("quantile_inf","mean","quantile_sup")

for(i in 1:I){
  resultats_finaux[i,] <- quantile(simulations[i,], probs = c(0.05,0.5,0.95))
}

{
  plot(1:I,
       table_region$rh98,
       xlab="i",
       ylab="rh98"
  )
  lines(1:I,resultats_finaux[,2],col="pink",lty=1)
  lines(1:I,resultats_finaux[,1],col="blue",lty=2)
  lines(1:I,resultats_finaux[,3],col="darkblue",lty=2)
}

hist(table_region$mean_precip,breaks=30)
indices_of_increasing_mean_precip <- sort(table_region$mean_precip, index.return=TRUE)$ix

plot(1:length(table_region$mean_precip),
     table_region$mean_precip[indices_of_increasing_mean_precip],
     xlab="indices ordonnés",
     ylab="mean_precip croissant")

resultats_finaux <- resultats_finaux[indices_of_increasing_mean_precip,]

{
  plot(1:I,
       table_region$rh98[indices_of_increasing_mean_precip],
       xlab="mean_precip croissant",
       ylab="rh98"
  )
  lines(1:I,resultats_finaux[,2],col="pink",lty=1)
  lines(1:I,resultats_finaux[,1],col="blue",lty=2)
  lines(1:I,resultats_finaux[,3],col="darkblue",lty=2)
}



