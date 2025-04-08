library(brms)
options(mc.cores = parallel::detectCores())
i=2
data <- readRDS("data_point_in_cells_10e4_prec_subsampled.RDS")

data_prec <- data[[i]]
prior <- c(set_prior("gamma(1,0.1)", class = "shape"))
##inference
Sys.time()
mod_rh98_prec <- brm(
  rh98 ~ (clay_percent_std + fire_freq_std|unique_id) + prec_std,
  data = data_prec,family = brmsfamily(family = "Gamma",link="log"),
  prior = NULL,warmup = 1*10**3, iter = 2*10**3,thin = 10,
  chains = 3, cores = 3, silent=2
)
Sys.time()
#30 min, + d'iter et maybe + de thin
saveRDS(mod_rh98_prec, file = paste("mod_rh98_prec",i,".RDS", sep =""))