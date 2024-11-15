# Libraries
library(fst)
library(rjags)
library(rstan)
library(stringr) 
library(brms)
library(corrplot)

# Rstan commands :
options(mc.cores = parallel::detectCores())


table_region <- readRDS( file.path(
  "transformed_data",
  "subsampled_6_ecoregions.RDS")
)

colnames(table_region)[13:15] <- c("fire_freq_std", 
                                   "mean_precip_std",
                                   "mean_temp_std")
table_region <- table_region[,-12]

mod_rh98 <- brm(
      formula = rh98 ~ fire_freq_std + mean_precip_std, #+ mean_temp_std,
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
                paste0("brms_regression_rh98_v2.RDS"))
)
      
  
 
mod_canopy_cover <- brm(
      formula = canopy_cover ~ fire_freq_std + mean_precip_std + mean_temp_std,
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
                paste0("brms_regression_canopy_cover.RDS")
              )
)
 

###plots
library(tidybayes)
library(dplyr)
library(ggplot2)
library(brms)

mod_canopy_cover <- readRDS(file = "outputs/brms_regression_canopy_cover.RDS")
mod_rh98 <- readRDS(file = "outputs/brms_regression_rh98_v2.RDS")

##coeff
#table_post_cc <- as.data.frame(mod_canopy_cover)
table_post_cc <- gather_draws(mod_canopy_cover, 
                              c(b_fire_freq_std, b_mean_temp_std,
                                b_mean_precip_std)
)

ggplot(data = table_post_cc, 
       aes(x = .value, y = .variable)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Value")+
  ylab("Predictor coefficient")+
  ggtitle("Canopy cover")
  #scale_fill_manual(values = c("gray80", "skyblue"))

table_post_rh98 <- gather_draws(mod_rh98, 
                              c(b_fire_freq_std, b_mean_temp_std,
                                b_mean_precip_std)
)

ggplot(data = table_post_rh98, 
       aes(x = .value, y = .variable)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Value")+
  ylab("Predictor coefficient")+
  ggtitle("RH 98")


##PPC
# predictions <- table_region %>%
#         select(c(canopy_cover,fire_freq_std, mean_precip_std, mean_temp_std)) %>%
#         add_predicted_draws(mod_canopy_cover, seed = 1234, ndraws = 100)

#canopy cover
table_pred_cc <- data.frame(predict(mod_canopy_cover, ndraws = 100, summary = T, probs = 0.5))

predictions_50_cc <- cbind(table_region, table_pred_cc$"Q50")

ggplot(data = predictions_50_cc, aes(x = canopy_cover, y = table_pred_cc$Q50)) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed")+
  lims(x = c(0,1), y = c(0,1))+
  xlab("Observed canopy cover")+
  ylab("Median predicted canopy cover")

#rh98
table_pred_rh98 <- data.frame(predict(mod_rh98, ndraws = 100, summary = T, probs = 0.5))

predictions_50_rh98 <- cbind(table_region, table_pred_rh98$"Q50")

ggplot(data = predictions_50_rh98, aes(x = rh98, y = table_pred_rh98$Q50)) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed")+
  lims(x = c(0,27), y = c(0,27))+
  xlab("Observed rh98")+
  ylab("Median predicted rh98")

##residual
pe_cc <- predictive_error(mod_canopy_cover, ndraws = 100)
mean_error_cc <- apply(X = sqrt(pe_cc^2), FUN = mean, MARGIN = 2)
pred_error_cc <- cbind(predictions_50_cc, mean_error_cc)

ggplot(data = pred_error_cc, aes(x = x_TRUE, y = y_TRUE))+
  geom_point(aes(color = mean_error_cc))+
  scale_color_viridis()+
  xlab("longitude")+
  ylab("latitude")+
  labs(color = "Mean error")+
  ggtitle("Canopy cover")

pe_rh98 <- predictive_error(mod_rh98, ndraws = 100)
mean_error_rh98 <- apply(X = sqrt(pe_rh98^2), FUN = mean, MARGIN = 2)
pred_error_rh98 <- cbind(predictions_50_rh98, mean_error_rh98)

ggplot(data = pred_error_rh98, aes(x = x_TRUE, y = y_TRUE))+
  geom_point(aes(color = mean_error_rh98))+
  scale_color_viridis()+
  xlab("longitude")+
  ylab("latitude")+
  labs(color = "Mean error")+
  ggtitle("RH98")
      