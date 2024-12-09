# Libraries
library(fst)
library(rjags)
library(rstan)
library(stringr) 
library(brms)
library(corrplot)
library(loo)

# Rstan commands :
options(mc.cores = parallel::detectCores())

table_region <- readRDS( file.path(
  "transformed_data",
  "subsampled_v3_10000.RDS")
)

mod_rh98_q50 <- brm(
     rh98_q50 ~ fire_freq_mean + precip_mean + mean_precip_carre + #fire_freq_std * mean_precip_std +
        clay_percent_mean,
      data = table_region, 
      family = brmsfamily(family = "Gamma",link="log"),
      
      prior = NULL,
      
      warmup = 2*10**3,
      iter = 10**4,
      thin = 10,
      
      chains = 3,
      cores = 3,          
      
      # control = list(adapt_delta = 0.95), 
      
      silent = 1 # full comments
)
    

saveRDS(mod_rh98_q50,
              file.path(
                "outputs",
                paste0("brms_regression_rh98q50_09_12.RDS"))
)
 
mod_cc_q90 <- brm(
      formula = cc_q90 ~ fire_freq_mean + precip_mean + clay_percent_mean,
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
      
      chains = 3,
      cores = 3,
      
      # control = list(adapt_delta = 0.95),
      
      silent = 1
      # full comments
)
    
saveRDS(mod_cc_q90,
              file.path(
                "outputs",
                paste0("brms_regression_ccq90_03_12.RDS")
              )
)
 
#cc = f(rh98)
mod_cc_rh98 <- brm(
  cc_q90 ~ rh98_q90,
  #+ mean_temp_std,
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
  
  chains = 3,
  cores = 3,          
  
  # control = list(adapt_delta = 0.95), 
  
  silent = 1 # full comments
)


saveRDS(mod_cc_rh98,
        file.path(
          "outputs",
          paste0("brms_regression_cc_frh98_q90_09_12.RDS"))
)

