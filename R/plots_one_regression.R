library(tidybayes)
library(dplyr)
library(ggplot2)
library(brms)
library(viridis)
library(rnaturalearth)
library(sf)
library(gridExtra)

#load data
table_region <- readRDS(file.path(
  "transformed_data","subsampled_cell_10e3_for_one_reglin.RDS"))
table_region <- table_region[-which(is.na(table_region$clay_percent_mean)),] #une ligne

world <- ne_countries()
africa <- world %>% filter(continent == "Africa")


#load brms fits
mod_rh98 <- readRDS(file = "outputs/brms_regression_rh98q90_22_05.RDS")

table_post_rh98 <- gather_draws(mod_rh98, 
                                c(b_fire_freq_mean,
                                  b_precip_mean,
                                  b_clay_percent_mean
                                  )
)

pdf(file.path("figures","one_reg_post_rh98.pdf"), height = 6, width = 6)
ggplot(data = table_post_rh98, 
       aes(x = .value, y = .variable)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Value")+
  ylab("Predictor coefficient")+
  ggtitle("RH 98")
dev.off()

table_pred_rh98 <- data.frame(predict(mod_rh98, ndraws = 50, summary = T, probs = 0.5))
predictions_rh98 <- cbind(table_region, table_pred_rh98$"Q50")

p1 <- ggplot(data = predictions_rh98, aes(x = rh98_q90, y = table_pred_rh98$Q50)) +
  geom_point(aes(color = fire_freq))+
  scale_color_viridis()+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed")+
  lims(x = c(0,30), y = c(0,30))+
  xlab("Observed q90 rh98")+
  ylab("Median predicted q90 rh98")

p2 <- ggplot(data = predictions_rh98, aes(x = rh98_q90, y = table_pred_rh98$Q50)) +
  geom_point(aes(color = mean_precip))+
  scale_color_viridis()+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed")+
  lims(x = c(0,30), y = c(0,30))+
  xlab("Observed q90 rh98")+
  ylab("Median predicted q90 rh98")

pdf(file.path("figures","one_reg_obs_vs_pred_rh98.pdf"), height = 8, width = 12)
grid.arrange(p1,p2, ncol = 2)
dev.off()

## a refaire
pe_rh98 <- predictive_error(mod_rh98, ndraws = 50)
mean_error_rh98 <- apply(X = sqrt(pe_rh98^2), FUN = mean, MARGIN = 2)
#signe de l'erreur
median_error_rh98 <- apply(X = pe_rh98, FUN = median, MARGIN = 2)
signe_error_rh98 <- as.factor(ifelse(median_error_rh98 >0, 1, -1))
pred_error_rh98 <- cbind(predictions_rh98, mean_error_rh98, signe_error_rh98,
                         percent_error = mean_error_rh98/predictions_rh98$rh98_q90*100
)
#que les erreurs < 100%
pred_error_rh98_red <- subset(pred_error_rh98, percent_error < 100)
pred_error_sf <- st_as_sf(pred_error_rh98_red, 
                          coords = c("x_TRUE", "y_TRUE"), crs = "+proj=longlat +datum=WGS84")

pdf(file.path("figures","one_reg_map_error_rh98.pdf"), height = 12, width = 12)
ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_sf(data=pred_error_sf, aes(color = percent_error, shape = signe_error_rh98))+
  scale_color_viridis()+
  scale_shape_manual(values=c(2, 1)) +
  coord_sf(xlim=c(-20, 40), ylim=c(-12, 20), expand=FALSE)+
  labs(color = "Percent error", shape = "Error sign")+
  ggtitle("Q90 RH98")+
theme(panel.background=element_rect(fill="slategray1"))
dev.off()

##canopy cover
mod_canopy_cover <- readRDS(file = "outputs/brms_regression_ccq90_22_05.RDS")
##coeff
table_post_cc <- gather_draws(mod_canopy_cover, 
                              c(b_fire_freq_mean,
                                b_precip_mean,
                                b_clay_percent_mean
                              )
)

pdf(file.path("figures","one_reg_post_cc.pdf"), height = 6, width = 6)
ggplot(data = table_post_cc, 
       aes(x = .value, y = .variable)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Value")+
  ylab("Predictor coefficient")+
  ggtitle("Canopy cover")
dev.off()

table_pred_cc <- data.frame(predict(mod_canopy_cover, ndraws = 50, summary = T, probs = 0.5))
predictions_cc <- cbind(table_region, table_pred_cc$"Q50")

p1 <- ggplot(data = predictions_cc, aes(x = cc_q90, y = table_pred_cc$Q50)) +
  geom_point(aes(color = fire_freq))+
  scale_color_viridis()+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed")+
  lims(x = c(0,1), y = c(0,1))+
  xlab("Observed q90 canopy cover")+
  ylab("Median predicted q90 canopy cover")

p2 <- ggplot(data = predictions_cc, aes(x = cc_q90, y = table_pred_cc$Q50)) +
  geom_point(aes(color = mean_precip))+
  scale_color_viridis()+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed")+
  lims(x = c(0,1), y = c(0,1))+
  xlab("Observed q90 canopy cover")+
  ylab("Median predicted q90 canopy cover")

pdf(file.path("figures","one_reg_obs_vs_pred_cc.pdf"), height = 8, width = 12)
grid.arrange(p1,p2, ncol = 2)
dev.off()

##a refaire
#mean error
pe_cc <- predictive_error(mod_canopy_cover, ndraws = 50)
mean_error_cc <- apply(X = sqrt(pe_cc^2), FUN = mean, MARGIN = 2)
pred_error_cc <- cbind(predictions_cc, mean_error_cc)
median_error_cc <- apply(X = pe_cc, FUN = median, MARGIN = 2)
signe_error_cc <- as.factor(ifelse(median_error_cc >0, 1, -1))
pred_error_cc <- cbind(predictions_cc, mean_error_cc, signe_error_cc,
                       percent_error = mean_error_cc/predictions_cc$cc_q90*100)

# #que les erreurs < 100%

 pred_error_cc_red <- subset(pred_error_cc, median_error_cc>0.5 | median_error_cc < -0.5)
pred_error_sf <- st_as_sf(pred_error_cc_red, 
                          coords = c("x_TRUE", "y_TRUE"), crs = "+proj=longlat +datum=WGS84")

pdf(file.path("figures","one_reg_map_error_cc.pdf"), height = 12, width = 12)
ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_sf(data=pred_error_sf, aes(color = mean_error_cc, shape = signe_error_cc))+
  scale_color_viridis()+
  scale_shape_manual(values=c(1, 2)) +
  coord_sf(xlim=c(-20, 40), ylim=c(-12, 20), expand=FALSE)+
  labs(color = "Mean error", shape = "Error sign")+
  ggtitle("Q90 canopy cover")+
  theme(panel.background=element_rect(fill="slategray1"))
dev.off()

#plot data
cc_sf <- st_as_sf(predictions_cc, 
                          coords = c("x_TRUE", "y_TRUE"), crs = "+proj=longlat +datum=WGS84")
ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_sf(data=cc_sf, aes(color = cc_q90))+
  scale_color_viridis()+
  coord_sf(xlim=c(-20, 40), ylim=c(-12, 20), expand=FALSE)+
  ggtitle("Q90 canopy cover")+
  theme(panel.background=element_rect(fill="slategray1"))

############# PAS REFAIT ###################
#cc ~ rh98
mod_cc_rh98 <- readRDS(file = "outputs/brms_regression_cc_frh98_q90_09_12.RDS")
table_pred <- data.frame(predict(mod_cc_rh98, ndraws = 100, summary = T, probs = 0.5))
predictions_50 <- cbind(table_region, table_pred$"Q50")
ggplot(data = predictions_50, aes(x = canopy_cover, y = table_pred$Q50)) +
  geom_point(aes(color = mean_precip))+
  scale_color_viridis()+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed")+
  lims(x = c(0,1), y = c(0,1))+
  xlab("Observed Q90 canopy cover")+
  ylab("Median predicted Q90 canopy cover")


####
ggplot(data = table_region, aes(x = mean_precip, y = fire_freq))+
  geom_point()+
  geom_hline(yintercept = 0.25, color="red", linetype="dashed")+
  geom_vline(xintercept = 1000, color="red", linetype="dashed")+
  ylab("Fire frequency")+
  xlab("Mean annual precipitation")

quadrant <- ifelse(table_region$fire_freq <0.25 & table_region$mean_precip <=1000, 1,
                   ifelse(table_region$fire_freq <0.25 & table_region$mean_precip >1000,2,
                          ifelse(table_region$fire_freq >0.25 & table_region$mean_precip >1000,3,4)
                   ))
table_region <- cbind(table_region, quadrant = as.factor(quadrant))                   
table_sf <- st_as_sf(table_region, coords = c("x_TRUE", "y_TRUE"),
                     crs = "+proj=longlat +datum=WGS84")
ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_sf(data=table_sf, aes(color = quadrant))+
  scale_color_viridis(discrete = T)+
  coord_sf(xlim=c(-20, 40), ylim=c(-12, 20), expand=FALSE)+
  #labs(color = "Percent error", shape = "Error sign")+
  ggtitle("Regimes")  


