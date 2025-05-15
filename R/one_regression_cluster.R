# Libraries
library(brms)


options(mc.cores = parallel::detectCores())

table_region <- readRDS("subsampled_cell_10e3_for_one_reglin.RDS")

mod_rh98_q90 <- brm(
  rh98_q90 ~ fire_freq_mean + precip_mean + mean_precip_carre + clay_percent_mean,
  data = table_region, family = brmsfamily(family = "lognormal"), prior = NULL, 
  warmup = 2*10**2, iter = 10**3,thin = 3,chains = 3,cores = 3,silent = 2
)

saveRDS(mod_rh98_q90,"brms_regression_rh98q90_07_05.RDS")


mod_cc_q90 <- brm(
  formula = cc_q90 ~ fire_freq_mean + precip_mean + clay_percent_mean,
  data = table_region, family = brmsfamily( family = "beta"), prior = NULL,
  warmup = 2*10**2, iter = 10**3, thin = 3, chains = 3, cores = 3, silent = 2
)

saveRDS(mod_cc_q90,"brms_regression_ccq90_07_05.RDS")


