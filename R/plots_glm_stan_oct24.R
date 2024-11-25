
###plots
library(tidybayes)
library(dplyr)
library(ggplot2)
library(brms)
library(viridis)

#load data
table_region <- readRDS( file.path(
  "transformed_data",
  "subsampled_6_ecoregions.RDS")
)

colnames(table_region)[13:15] <- c("fire_freq_std", 
                                   "mean_precip_std",
                                   "mean_temp_std")
table_region <- table_region[,-12]
table_region <- cbind(table_region, 
                      mean_precip_carre = (table_region$mean_precip_std)^2)

#load brms fits
mod_canopy_cover <- readRDS(file = "outputs/brms_regression_canopy_cover.RDS")
mod_rh98 <- readRDS(file = "outputs/brms_regression_rh98_v3.RDS")

##coeff

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
                                c(b_fire_freq_std, b_mean_precip_carre,
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
  geom_point(aes(color = mean_precip))+
  scale_color_viridis()+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed")+
  lims(x = c(0,27), y = c(0,27))+
  xlab("Observed rh98")+
  ylab("Median predicted rh98")


table_pred_cc <- data.frame(predict(mod_cc_rh98, ndraws = 100, summary = T, probs = 0.5))

predictions_50_cc <- cbind(table_region, table_pred_cc$"Q50")

ggplot(data = predictions_50_cc, aes(x = canopy_cover, y = table_pred_cc$Q50)) +
  geom_point()+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed")+
  lims(x = c(0,1), y = c(0,1))+
  xlab("Observed canopy cover")+
  ylab("Median predicted canopy cover")


##residual
pe_cc <- predictive_error(mod_canopy_cover, ndraws = 100)
pe_cc <- predictive_error(mod_cc_rh98, ndraws = 100)

#mean error
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

#signe de l'erreur
median_error_rh98 <- apply(X = pe_rh98, FUN = median, MARGIN = 2)
signe_error_rh98 <- as.factor(ifelse(median_error_rh98 >0, 1, -1))

pred_error_rh98 <- cbind(predictions_50_rh98, mean_error_rh98, signe_error_rh98,
                         percent_error = mean_error_rh98/pred_error_rh98$rh98*100
)

ggplot(data = pred_error_rh98, aes(x = x_TRUE, y = y_TRUE))+
  geom_point(aes(color = mean_error_rh98, shape = signe_error_rh98))+
  scale_color_viridis()+
  scale_shape_manual(values=c(16, 17))+
  xlab("longitude")+
  ylab("latitude")+
  labs(color = "Mean error", shape = "Error sign")+
  ggtitle("RH98")

#que les erreurs < 100% (on enlève 535 pts, ie 13%)
pred_error_rh98_red <- subset(pred_error_rh98, percent_error < 100)


africa <- world %>% filter(continent == "Africa")
pred_error_sf <- st_as_sf(pred_error_rh98_red, coords = c("x_TRUE", "y_TRUE"),
                          crs = "+proj=longlat +datum=WGS84")

ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_sf(data=pred_error_sf, aes(color = percent_error, shape = signe_error_rh98))+
  scale_color_viridis()+
  scale_shape_manual(values=c(17, 16)) +
  coord_sf(xlim=c(-20, 40), ylim=c(-12, 20), expand=FALSE)+
  labs(color = "Percent error", shape = "Error sign")+
  ggtitle("RH98")
#theme(panel.background=element_rect(fill="slategray1"))

