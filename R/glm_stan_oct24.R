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
  "subsampled_6_ecoregions_rh98sup3.RDS")
)

mod_rh98 <- brm(
     rh98 ~ fire_freq_std + mean_precip_std + mean_precip_carre + #fire_freq_std * mean_precip_std +
        clay_percent_std,
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
    

saveRDS(mod_rh98,
              file.path(
                "outputs",
                paste0("brms_regression_rh98_03_12.RDS"))
)
      
  
 
mod_canopy_cover <- brm(
      formula = canopy_cover ~ fire_freq_std + mean_precip_std + clay_percent_std,
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
    
saveRDS(mod_canopy_cover,
              file.path(
                "outputs",
                paste0("brms_regression_canopy_cover_03_12.RDS")
              )
)
 
#cc = f(rh98)
mod_cc_rh98 <- brm(
  canopy_cover ~ rh98,
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
          paste0("brms_regression_cc_frh98_03_12.RDS"))
)

