# Libraries
library(brms)


options(mc.cores = parallel::detectCores())

for (i in 1:4){
table_region <- readRDS(file.path("transformed_data",
                      paste0("subsampled_cell_10e3_for_one_reglin_prec",i,".RDS")
))

mod_rh98_q90 <- brm(
  rh98_q90 ~ fire_freq_mean + precip_mean + clay_percent_mean,
  data = table_region, family = brmsfamily(family = "lognormal"), prior = NULL, 
  warmup = 10**3, iter = 10**4,thin = 3,chains = 3,cores = 3,silent = 2
)

saveRDS(mod_rh98_q90,
        file.path("outputs",paste0("brms_regression_rh98q90_prec",i,".RDS"))
)


mod_cc_q90 <- brm(
  formula = cc_q90 ~ fire_freq_mean + precip_mean + clay_percent_mean,
  data = table_region, family = brmsfamily( family = "beta"), prior = NULL,
  warmup = 10**3, iter = 10**4, thin = 3, chains = 3, cores = 3, silent = 2
)

saveRDS(mod_cc_q90,
        file.path("outputs",paste0("brms_regression_ccq90_prec",i,".RDS"))
)

}
