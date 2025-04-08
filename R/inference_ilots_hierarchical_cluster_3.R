library(brms)
options(mc.cores = parallel::detectCores())
i=3
data <- readRDS("data_point_in_cells_10e4_prec_subsampled.RDS")

data_prec <- data[[i]]

##inference
Sys.time()
mod_rh98_prec <- brm(
  rh98 ~ (clay_percent_std + fire_freq_std|unique_id) + prec_std,
  data = data_prec,family = brmsfamily(family = "lognormal"),
  prior = NULL,warmup = 1.5*10**3, iter = 3.5*10**3,thin = 3,
  chains = 3, cores = 3, silent=2
)
Sys.time()
saveRDS(mod_rh98_prec, file = paste("mod_rh98_prec",i,".RDS", sep =""))
