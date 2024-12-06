
###plots
library(tidybayes)
library(dplyr)
library(ggplot2)
library(brms)
library(viridis)
library(rnaturalearth)
library(sf)

#load data
table_region <- readRDS( file.path(
  "transformed_data",
  "subsampled_6_ecoregions_rh98sup3.RDS")
)

#load brms fits
mod_canopy_cover <- readRDS(file = "outputs/brms_regression_canopy_cover_03_12.RDS")
mod_rh98 <- readRDS(file = "outputs/brms_regression_rh98_03_12.RDS")
mod_cc_rh98 <- readRDS(file = "outputs/brms_regression_cc_frh98_03_12.RDS")

##coeff

table_post_cc <- gather_draws(mod_canopy_cover, 
                              c(b_fire_freq_std, b_mean_precip_std)
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
  geom_point(aes(color = fire_freq))+
  scale_color_viridis()+
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
  lims(x = c(0,28), y = c(0,28))+
  xlab("Observed rh98")+
  ylab("Median predicted rh98")


table_pred_cc <- data.frame(predict(mod_cc_rh98, ndraws = 100, summary = T, probs = 0.5))

predictions_50_cc <- cbind(table_region, table_pred_cc$"Q50")

ggplot(data = predictions_50_cc, aes(x = canopy_cover, y = table_pred_cc$Q50)) +
  geom_point(aes(color = mean_precip))+
  scale_color_viridis()+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed")+
  lims(x = c(0,1), y = c(0,1))+
  xlab("Observed canopy cover")+
  ylab("Median predicted canopy cover")


##residual
#pe_cc <- predictive_error(mod_canopy_cover, ndraws = 100)

# ggplot(data = pred_error_cc, aes(x = x_TRUE, y = y_TRUE))+
#   geom_point(aes(color = mean_error_cc))+
#   scale_color_viridis()+
#   xlab("longitude")+
#   ylab("latitude")+
#   labs(color = "Mean error")+
#   ggtitle("Canopy cover")

pe_rh98 <- predictive_error(mod_rh98, ndraws = 100)
mean_error_rh98 <- apply(X = sqrt(pe_rh98^2), FUN = mean, MARGIN = 2)

#signe de l'erreur
median_error_rh98 <- apply(X = pe_rh98, FUN = median, MARGIN = 2)
signe_error_rh98 <- as.factor(ifelse(median_error_rh98 >0, 1, -1))

pred_error_rh98 <- cbind(predictions_50_rh98, mean_error_rh98, signe_error_rh98,
                         percent_error = mean_error_rh98/predictions_50_rh98$rh98*100
)

# ggplot(data = pred_error_rh98, aes(x = x_TRUE, y = y_TRUE))+
#   geom_point(aes(color = mean_error_rh98, shape = signe_error_rh98))+
#   scale_color_viridis()+
#   scale_shape_manual(values=c(16, 17))+
#   xlab("longitude")+
#   ylab("latitude")+
#   labs(color = "Mean error", shape = "Error sign")+
#   ggtitle("RH98")

#que les erreurs < 100% (on enlève 535 pts, ie 13%)
pred_error_rh98_red <- subset(pred_error_rh98, percent_error < 100)

world <- ne_countries()
africa <- world %>% filter(continent == "Africa")
pred_error_sf <- st_as_sf(pred_error_rh98, coords = c("x_TRUE", "y_TRUE"),
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

pe_cc <- predictive_error(mod_cc_rh98, ndraws = 100)

#mean error
mean_error_cc <- apply(X = sqrt(pe_cc^2), FUN = mean, MARGIN = 2)
pred_error_cc <- cbind(predictions_50_cc, mean_error_cc)


median_error_cc <- apply(X = pe_cc, FUN = median, MARGIN = 2)
signe_error_cc <- as.factor(ifelse(median_error_cc >0, 1, -1))

pred_error_cc <- cbind(predictions_50_cc, mean_error_cc, signe_error_cc,
                         percent_error = mean_error_cc/predictions_50_cc$canopy_cover*100
)

pred_error_cc_red <- subset(pred_error_cc, percent_error < 100)

world <- ne_countries()
africa <- world %>% filter(continent == "Africa")
pred_error_sf <- st_as_sf(pred_error_cc_red, coords = c("x_TRUE", "y_TRUE"),
                          crs = "+proj=longlat +datum=WGS84")

ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_sf(data=pred_error_sf, aes(color = percent_error, shape = signe_error_cc))+
  scale_color_viridis()+
  scale_shape_manual(values=c(17, 16)) +
  coord_sf(xlim=c(-20, 40), ylim=c(-12, 20), expand=FALSE)+
  labs(color = "Percent error", shape = "Error sign")+
  ggtitle("Canopy cover")

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
