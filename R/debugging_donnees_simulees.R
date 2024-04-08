
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
# [1] "Guinean_forest-savanna_regression_rh98.RDS"
mcmc_output <- readRDS(list_of_ouputs[1])
summary(mcmc_output)

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

# make_stancode(formula = rh98 ~ fire_freq_std + mean_precip_std,
#               data = table_region,
#               family = brmsfamily(family = "Gamma",link="log"))

stancode(mcmc_output)

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

# for all i,j we make a gamma draw
for(i in 1:I){
  print(paste("i",i,"/",I))
  for(j in 1:J){
    # mu += Intercept + Xc * b;
    # mu = exp(mu);
    # target += gamma_lpdf(Y | shape, shape ./ mu);
    
    shape <- mcmc_values[j,"shape"]
    
    simulations[i,j] <- rgamma(n=1,shape = shape, scale = exp(linear_predictors_for_one_beta_draw_per_column[i,j])/shape)
  }
print(paste("pour",J,"j"))
}


# hist(simulations[1,],breaks=30)
# quantile(simulations[1,], probs = c(0.05,0.5,0.95))

table_region <- cbind(table_region,as.data.frame(matrix(nrow=I,ncol=3)))
colnames(table_region)[ncol(table_region)-2] = c("pred_rh98_quantile_inf")
colnames(table_region)[ncol(table_region)-1] = c("pred_rh98_mean")
colnames(table_region)[ncol(table_region)] = c("pred_rh98_quantile_sup")

for(i in 1:I){
  table_region[i,c("pred_rh98_quantile_inf",
                   "pred_rh98_mean",
                   "pred_rh98_quantile_sup")] <- quantile(simulations[i,], probs = c(0.05,0.5,0.95))
}

{
  plot(1:I,
       table_region$rh98,
       xlab="i",
       ylab="rh98"
  )
  lines(1:I,table_region[,"pred_rh98_mean"],col="pink",lty=1)
  lines(1:I,table_region[,"pred_rh98_quantile_inf"],col="blue",lty=2)
  lines(1:I,table_region[,"pred_rh98_quantile_sup"],col="darkblue",lty=2)
}

hist(table_region$rh98,breaks=50,xlim=c(0,30))
hist(table_region$pred_rh98_mean,breaks=50,xlim=c(0,30))
{plot(table_region$rh98,table_region$pred_rh98_mean,xlim=c(0,30),ylim=c(0,30))
abline(0,1,col="red")}

indices_of_increasing_rh98 <- sort(table_region$rh98, index.return=TRUE)$ix
ordered_table_region <- table_region[indices_of_increasing_rh98,]

plot(1:length(ordered_table_region$rh98),
     ordered_table_region$rh98,
     xlab="indices ordonnés",
     ylab="rh98 croissant")


{
  plot(1:I,
       ordered_table_region$rh98,
       xlab="i",
       ylab="rh98"
  )
  lines(1:I,ordered_table_region[,"pred_rh98_mean"],col="pink",lty=1)
  lines(1:I,ordered_table_region[,"pred_rh98_quantile_inf"],col="blue",lty=2)
  lines(1:I,ordered_table_region[,"pred_rh98_quantile_sup"],col="darkblue",lty=2)
}

#####################################################################################################################
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

stancode(mcmc_output)

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
require(gtools) # for inv.logit

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


table_region <- cbind(table_region,as.data.frame(matrix(nrow=I,ncol=3)))
colnames(table_region)[ncol(table_region)-2] = c("pred_canopy_cover_quantile_inf")
colnames(table_region)[ncol(table_region)-1] = c("pred_canopy_cover_mean")
colnames(table_region)[ncol(table_region)] = c("pred_canopy_cover_quantile_sup")

for(i in 1:I){
  table_region[i,c("pred_canopy_cover_quantile_inf",
                   "pred_canopy_cover_mean",
                   "pred_canopy_cover_quantile_sup")] <- quantile(simulations[i,], probs = c(0.05,0.5,0.95))
}

{
  plot(1:I,
       table_region$canopy_cover,
       xlab="i",
       ylab="canopy_cover"
  )
  lines(1:I,table_region[,"pred_canopy_cover_mean"],col="pink",lty=1)
  lines(1:I,table_region[,"pred_canopy_cover_quantile_inf"],col="blue",lty=2)
  lines(1:I,table_region[,"pred_canopy_cover_quantile_sup"],col="darkblue",lty=2)
}

hist(table_region$canopy_cover,breaks=50,xlim=c(0,1))
hist(table_region$pred_canopy_cover_mean,breaks=50,xlim=c(0,1))
{plot(table_region$canopy_cover,table_region$pred_canopy_cover_mean,xlim=c(0,1),ylim=c(0,1))
  abline(0,1,col="red")}

indices_of_increasing_canopy_cover <- sort(table_region$canopy_cover, index.return=TRUE)$ix
ordered_table_region <- table_region[indices_of_increasing_canopy_cover,]

plot(1:length(ordered_table_region$canopy_cover),
     ordered_table_region$canopy_cover,
     xlab="indices ordonnés",
     ylab="canopy_cover croissant")


{
  plot(1:I,
       ordered_table_region$rh98,
       xlab="i",
       ylab="canopy_cover"
  )
  lines(1:I,table_region[,"pred_canopy_cover_mean"],col="pink",lty=1)
  lines(1:I,table_region[,"pred_canopy_cover_quantile_inf"],col="blue",lty=2)
  lines(1:I,table_region[,"pred_canopy_cover_quantile_sup"],col="darkblue",lty=2)
}
