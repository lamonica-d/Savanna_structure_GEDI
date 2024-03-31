
# Cleaning the environment
rm(list=ls())
# Getting the paths
source("R/paths.R")
path_to_R_folder = file.path(
  path_to_Savanna_structure_GEDI_folder,
  "R"
)

# Libraries
library(fst)
library(rjags)
library(rstan)
stan_version()
library(stringr) 
library(brms)

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

(vect_names <- str_sub(list.files(path = path_to_GEDI_raw_data),end = -5))

for (name in vect_names[c(10,17,20)]){

table_region <- readRDS(
  file.path(
    path_to_Savanna_structure_GEDI_folder,
    "transformed_data",
    paste0(name,".RDS")
  )
)
print(nrow(table_region))


# prior
default_prior = get_prior(
  formula = rh98 ~ fire_freq_std + mean_precip_std,
  
  data = table_region,
  
  family = brmsfamily(family = "Gamma")
  # no info about the links in (*)
  # unlike the beta inflated
)

prior_1 = c(
  prior(
    normal(1400,100),
    class="b",
    coef = mean_precip
  ),
  
  prior(
    normal(26,4),
    class="b",
    coef = mean_temp
  ),
  
  prior(
    normal(0.25,0.5),
    class="b",
    coef = fire_freq
  )
)

start <- Sys.time()
print(start)

mod_rh98 <- brm(
  
  formula = rh98 ~ mean_precip + mean_temp  + fire_freq,
  
  data = table_region,
  
  family = brmsfamily(family = "Gamma"),
  # no info about the links in (*)
  # unlike the beta inflated 
  
  prior = prior_1,
  
  warmup = 10**3,
  iter = 5*10**3,
  thin = 10,
  
  # to save/load the file automatically
  
  # file = file.path(path_to_GEDI_raw_data,
  #                  "outputs",
  #                  "table_Guinean_1.RDS"),
  
  chains = 3,
  cores = 3,          
  
  # control = list(adapt_delta = 0.95), 
  
  silent = 0
  # full comments
)

print(Sys.time() - start)

saveRDS(mod_rh98,
        file.path(
          path_to_Savanna_structure_GEDI_folder,
          "outputs",
          paste0(name,"_regression_rh98.RDS")
        )
)

default_prior = get_prior(
  formula = canopy_cover ~ mean_precip + mean_temp  + fire_freq,
  
  data = table_region,
  
  family = brmsfamily(
    family = "zero_inflated_beta",
    link = "logit",
    link_phi = "log",
    link_zi = "logit"
  )
)


prior_2 = c(
  prior(
    normal(1400,100),
    class="b",
    coef = mean_precip
  ),
  
  prior(
    normal(26,4),
    class="b",
    coef = mean_temp
  ),
  
  prior(
    normal(0.25,0.5),
    class="b",
    coef = fire_freq
  )
)


start <- Sys.time()
print(start)

mod_canopy_cover <- brm(
  
  formula = canopy_cover ~ mean_precip + mean_temp  + fire_freq,
  
  data = table_region,
  
  family = brmsfamily(
    family = "zero_inflated_beta",
    link = "logit",
    link_phi = "log",
    link_zi = "logit"
  ),
  
  prior = prior_2,
  
  warmup = 10**3,
  iter = 5*10**3,
  thin = 10,
  
  # to save/load the file automatically
  
  # file = file.path(path_to_GEDI_raw_data,
  #                  "outputs",
  #                  "table_Guinean_2.RDS"),
  
  chains = 3,
  cores = 3,          
  
  # control = list(adapt_delta = 0.95), 
  
  silent = 0
  # full comments
)

print(Sys.time() - start)

saveRDS(mod_canopy_cover,
        file.path(
          path_to_Savanna_structure_GEDI_folder,
          "outputs",
          paste0(name,"_regression_canopy_cover.RDS")
        )
)

}
