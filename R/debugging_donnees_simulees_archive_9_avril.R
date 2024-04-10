
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

{
  table_region <- readRDS(
                        file.path(
                          path_to_Savanna_structure_GEDI_folder,
                          "transformed_data",
                          paste0("Guinean_forest-savanna",".RDS"))
                        )

  table_region <- cbind(subset(table_region,select=rh98),
                        as.data.frame(matrix(nrow=nrow(table_region),ncol=3)),
                        subset(table_region,select=-rh98)
                        )
  colnames(table_region)[2] = c("pred_rh98_quantile_inf")
  colnames(table_region)[3] = c("pred_rh98_median")
  colnames(table_region)[4] = c("pred_rh98_quantile_sup")
}

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

# hist(simulations[1,],breaks = 30)
# quantile(simulations[1,], probs = c(0.05,0.5,0.95))

{
for(i in 1:I){
  table_region[i,c("pred_rh98_quantile_inf",
                   "pred_rh98_median",
                   "pred_rh98_quantile_sup")] <- quantile(simulations[i,], probs = c(0.05,0.5,0.95))
  }


  plot(1:I,
       table_region$rh98,
       xlab="i",
       ylab="rh98"
  )
  lines(1:I,table_region[,"pred_rh98_median"],col="pink",lty=1)
  lines(1:I,table_region[,"pred_rh98_quantile_inf"],col="blue",lty=2)
  lines(1:I,table_region[,"pred_rh98_quantile_sup"],col="darkblue",lty=2)
}

{

indices_of_increasing_rh98 <- sort(table_region$rh98, index.return=TRUE)$ix
ordered_table_region <- table_region[indices_of_increasing_rh98,]

  plot(1:I,
       ordered_table_region$rh98,
       xlab="i",
       ylab="rh98"
  )
  lines(1:I,ordered_table_region[,"pred_rh98_median"],col="pink",lty=1)
  lines(1:I,ordered_table_region[,"pred_rh98_quantile_inf"],col="blue",lty=2)
  lines(1:I,ordered_table_region[,"pred_rh98_quantile_sup"],col="darkblue",lty=2)
}

####### Sur les n premières valeurs

n = 100
{
  plot(1:n,
       table_region$rh98[1:n],
       xlab="i",
       ylab="rh98",
       ylim=c(0,max(table_region$rh98[1:n]))
  )
  lines(1:n,table_region[1:n,"pred_rh98_median"],col="pink",lty=1)
  lines(1:n,table_region[1:n,"pred_rh98_quantile_inf"],col="blue",lty=2)
  lines(1:n,table_region[1:n,"pred_rh98_quantile_sup"],col="darkblue",lty=2)

  indices_of_increasing_rh98 <- sort(table_region$rh98, index.return=TRUE)$ix
  ordered_table_region <- table_region[indices_of_increasing_rh98,]
  
  plot(1:n,
       ordered_table_region$rh98[1:n],
       xlab="i",
       ylab="rh98",
       ylim=c(0,max(table_region$rh98[1:n]))
  )
  lines(1:n,ordered_table_region[1:n,"pred_rh98_median"],col="pink",lty=1)
  lines(1:n,ordered_table_region[1:n,"pred_rh98_quantile_inf"],col="blue",lty=2)
  lines(1:n,ordered_table_region[1:n,"pred_rh98_quantile_sup"],col="darkblue",lty=2)
}
#### fin du truc sur les n premières valeurs

indices_of_increasing_rh98[1:5]
table_region[indices_of_increasing_rh98[1:5],"pred_rh98_median"]
head(ordered_table_region[,"pred_rh98_median"],5)

# Vraies valeurs
mean(table_region$rh98)
sd(table_region$rh98)

# Comparaison avec la simulation 1
j = 1
simu <- simulations[,j]

mean(simu)
mean(table_region$rh98)

sd(simu)
sd(table_region$rh98)

hist(table_region$rh98,
     freq=FALSE,
     breaks=50,
     xlim=c(0,30)
     )

hist(simulations[1,],
     freq=FALSE,
     breaks=50,
     xlim=c(0,30)
     )

####################

for(ok in sample(1:J,10,replace=FALSE)){
{plot(table_region$rh98,
      simulations[,ok],
      xlim=c(0,30),
      ylim=c(0,30),
      main = paste("betas n°",ok)
      )
      abline(0,1,col="red")
}
}

##################################

{
moyennes <- vector(length = J)
ecarts_types <- vector(length = J)

for(j in 1:J){
  moyennes[j] <- mean(simulations[,j])
  ecarts_types[j] <- sd(simulations[,j])
  }

{hist(moyennes,
      freq=FALSE,
      breaks=50,
      xlim=c(min(moyennes)-1,max(moyennes)+1),
      main=paste(round(mean(moyennes),3),"; simu=",round(mean(simu),3))
      )
  abline(v=mean(moyennes),
         col="steelblue"
  )
abline(v=mean(simu),
       col="blue"
       )
}

{hist(ecarts_types,
      freq=FALSE,
      breaks=50,
      xlim=c(min(ecarts_types)-1,max(ecarts_types)+1),
      main=paste(round(mean(ecarts_types),3),"; simu=",round(sd(simu),3))
)
  abline(v=mean(ecarts_types),
         col="orange"
  )
  abline(v=sd(simu),
         col="green"
  )
}

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

{
  table_region <- readRDS(
    file.path(
      path_to_Savanna_structure_GEDI_folder,
      "transformed_data",
      paste0("Guinean_forest-savanna",".RDS"))
  )
  
  table_region <- cbind(subset(table_region,select=canopy_cover),
                        as.data.frame(matrix(nrow=nrow(table_region),ncol=3)),
                        subset(table_region,select=-canopy_cover)
  )
  colnames(table_region)[2] = c("pred_canopy_cover_quantile_inf")
  colnames(table_region)[3] = c("pred_canopy_cover_median")
  colnames(table_region)[4] = c("pred_canopy_cover_quantile_sup")
}

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

{
  
  for(i in 1:I){
  table_region[i,c("pred_canopy_cover_quantile_inf",
                   "pred_canopy_cover_median",
                   "pred_canopy_cover_quantile_sup")] <- quantile(simulations[i,], probs = c(0.05,0.5,0.95))
  }


  plot(1:I,
       table_region$canopy_cover,
       xlab="i",
       ylab="canopy_cover"
  )
  lines(1:I,table_region[,"pred_canopy_cover_median"],col="pink",lty=1)
  lines(1:I,table_region[,"pred_canopy_cover_quantile_inf"],col="blue",lty=2)
  lines(1:I,table_region[,"pred_canopy_cover_quantile_sup"],col="darkblue",lty=2)
}

{
  
  indices_of_increasing_canopy_cover <- sort(table_region$canopy_cover, index.return=TRUE)$ix
  ordered_table_region <- table_region[indices_of_increasing_canopy_cover,]
  
  plot(1:I,
       ordered_table_region$canopy_cover,
       xlab="i",
       ylab="canopy_cover"
  )
  lines(1:I,ordered_table_region[,"pred_canopy_cover_median"],col="pink",lty=1)
  lines(1:I,ordered_table_region[,"pred_canopy_cover_quantile_inf"],col="blue",lty=2)
  lines(1:I,ordered_table_region[,"pred_canopy_cover_quantile_sup"],col="darkblue",lty=2)
}

####### Sur les n premières valeurs

n = 100
{
  plot(1:n,
       table_region$canopy_cover[1:n],
       xlab="i",
       ylab="canopy_cover",
       ylim=c(0,max(table_region$canopy_cover[1:n]))
  )
  lines(1:n,table_region[1:n,"pred_canopy_cover_median"],col="pink",lty=1)
  lines(1:n,table_region[1:n,"pred_canopy_cover_quantile_inf"],col="blue",lty=2)
  lines(1:n,table_region[1:n,"pred_canopy_cover_quantile_sup"],col="darkblue",lty=2)
  
  indices_of_increasing_canopy_cover <- sort(table_region$canopy_cover, index.return=TRUE)$ix
  ordered_table_region <- table_region[indices_of_increasing_canopy_cover,]
  
  plot(1:n,
       ordered_table_region$canopy_cover[1:n],
       xlab="i",
       ylab="canopy_cover",
       ylim=c(0,max(table_region$canopy_cover[1:n]))
  )
  lines(1:n,ordered_table_region[1:n,"pred_canopy_cover_median"],col="pink",lty=1)
  lines(1:n,ordered_table_region[1:n,"pred_canopy_cover_quantile_inf"],col="blue",lty=2)
  lines(1:n,ordered_table_region[1:n,"pred_canopy_cover_quantile_sup"],col="darkblue",lty=2)
}
#### fin du truc sur les n premières valeurs

indices_of_increasing_canopy_cover[1:5]
table_region[indices_of_increasing_canopy_cover[1:5],"pred_canopy_cover_median"]
head(ordered_table_region[,"pred_canopy_cover_median"],5)

# Vraies valeurs
mean(table_region$canopy_cover)
sd(table_region$canopy_cover)

# Comparaison avec la simulation 1
j = 1
simu <- simulations[,j]

mean(simu)
mean(table_region$canopy_cover)

sd(simu)
sd(table_region$canopy_cover)

for(j in sample(1:J,10,replace=FALSE)){
  
  hist(table_region$canopy_cover,
       freq=FALSE,
       breaks=50,
       xlim=c(0,1),
       main="true canopy_cover"
  )
  
  hist(simulations[,j],
       freq=FALSE,
       breaks=50,
       xlim=c(0,1),
       main = paste("betas n°",j)
  )
  
  print(mean(simu))
  print(sd(simu))

  {plot(table_region$canopy_cover,
        simulations[,j],
        xlim=c(0,1),
        ylim=c(0,1),
        main = paste("betas n°",j)
  )
    abline(0,1,col="red")
  }
}

##################################

{
  moyennes <- vector(length = J)
  ecarts_types <- vector(length = J)
  
  for(j in 1:J){
    moyennes[j] <- mean(simulations[,j])
    ecarts_types[j] <- sd(simulations[,j])
  }
  
  {hist(moyennes,
        freq=FALSE,
        breaks=50,
        xlim=c(min(moyennes)-0.05,max(moyennes)+0.05),
        main=paste(round(mean(moyennes),3),"; simu=",round(mean(simu),3))
  )
    abline(v=mean(moyennes),
           col="steelblue",
           lty=2
    )
    abline(v=mean(simu),
           col="blue"
    )
  }
  
  {hist(ecarts_types,
        freq=FALSE,
        breaks=50,
        xlim=c(min(ecarts_types)-0.05,max(ecarts_types)+0.05),
        main=paste(round(mean(ecarts_types),3),"; simu=",round(sd(simu),3))
  )
    abline(v=mean(ecarts_types),
           col="orange",
           lty=2
    )
    abline(v=sd(simu),
           col="green",
           lty = 6
    )
  }
  
}
