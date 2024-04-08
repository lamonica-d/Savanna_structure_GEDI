#first analyse global model

# Cleaning the environment
rm(list=ls())
# Getting the paths
source("paths.R")
source("R/paths.R")
# Libraries
library(fst)
library(rjags)
library(stringr) 

vect_names = c("Guinean_forest-savanna","West_Sudanian","Sahelian_Acacia")
name = "Guinean_forest-savanna"

save_rds_files = FALSE

#load data
table_region <- readRDS(
    file.path(
      path_to_Savanna_structure_GEDI_folder,
      "transformed_data",
      paste0(name,".RDS"))
  )


fire_freq_nozero <- table_region$fire_freq
fire_freq_nozero[fire_freq_nozero == 0] <- 10**-4

canopy_cover_nozero <- table_region$canopy_cover
canopy_cover_nozero[canopy_cover_nozero == 0] <- 10**-4
# ne vaut-il pas mieux faire un test type abs()<10**-14 ?

table_region <- cbind(table_region, fire_freq_nozero = fire_freq_nozero,
                      canopy_cover_nozero = canopy_cover_nozero)
head(table_region,2)

data <- list(N = nrow(table_region), 
             rh98 = table_region$rh98,
             fire_freq = table_region$fire_freq_nozero, 
             canopy_cover = table_region$canopy_cover_nozero,
             prec_input = table_region$mean_precip_std,
             ff_input = table_region$fire_freq_std, 
             cc_input = scale(table_region$canopy_cover, scale = T, center = T)[1:nrow(table_region)]
) 

# model and jags inference
model1 <-  "
model {
  
  for (i in 1:N){
    rh98[i] ~ dgamma(alpha, rate[i])
    rate[i] <- exp(a[1] + a[2] * prec_input[i] + a[3] * ff_input[i])

    canopy_cover[i] ~ dbeta(mu[i]*phi1, (1-mu[i])*phi1)
    logit(mu[i]) <- b[1] + b[2] * prec_input[i] + b[3] * ff_input[i]

    logit(grassB[i]) <- c[1] + c[2] * prec_input[i] + c[3] * cc_input[i]
    fire_freq[i] ~ dbeta(grassB[i]*phi2, (1-grassB[i])*phi2)

  }#data

#priors
log10alpha ~ dunif(-3,2)
log10phi1 ~ dunif(-2,2)
log10phi2 ~ dunif(-2,2)

alpha <- 10^log10alpha
phi1 <- 10^log10phi1
phi2 <- 10^log10phi2

  for (i in 1:3){
    a[i] ~ dnorm(0,1/(2*2))
    b[i] ~ dnorm(0,1/(2*2))
    c[i] ~ dnorm(0,1/(2*2))
  }
}
"


inits1 <- list(list(a = rnorm(3,mean = 0, sd = 2), b = rnorm(3,mean = 0, sd = 2), 
                    c = rnorm(3,mean = 0, sd = 2), log10alpha = runif(1, -3, 2),
                    log10phi1 = runif(1, 0, 1), log10phi2 = runif(1, 0, 1)),
               list(a = rnorm(3,mean = 0, sd = 2), b = rnorm(3,mean = 0, sd = 2), 
                    c = rnorm(3,mean = 0, sd = 2), log10alpha = runif(1, -3, 2),
                    log10phi1 = runif(1, 0, 1), log10phi2 = runif(1, 0, 1)),
               list(a = rnorm(3,mean = 0, sd = 2), b = rnorm(3,mean = 0, sd = 2), 
                    c = rnorm(3,mean = 0, sd = 2), log10alpha = runif(1, -3, 2),
                    log10phi1 = runif(1, 0, 1), log10phi2 = runif(1, 0, 1))
)

m1 <- rjags::jags.model(textConnection(model1), inits = inits1, 
                        data = data, n.chains = length(inits1))
update(m1, 1000)
mcmc1 <- rjags::coda.samples(m1, variable.names = c("a" , "b", "c" , "log10alpha",
                                                    "log10phi1", "log10phi2"),
                             n.iter = 5000, thin = 5) #given autocor thin = 20-40 should be good

#first checks
gelman.diag(mcmc1)
autocorr.plot(mcmc1[[1]], ask = "N")
plot(mcmc1, trace = T)
mtot1 <- as.data.frame(as.matrix(mcmc1))

#quick boxplots of regression coefficients
par(mfrow = c(1,1), ask = "N")
boxplot(mtot1[,1:9], las = 1)
abline(h = 0, lty = 3, col = "red")
boxplot(mtot1[,10:12], las = 1)

