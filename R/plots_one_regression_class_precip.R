library(tidybayes)
library(dplyr)
library(ggplot2)
library(brms)
library(viridis)
library(rnaturalearth)
library(sf)

#load data

table_region <- tibble()
table_post_rh98 <- tibble()
table_post_cc <- tibble()

for (i in 1:4){
  temp <- readRDS(file.path("transformed_data",
                                  paste0("subsampled_cell_10e3_for_one_reglin_prec",i,".RDS")
))
  #temp <- temp[-which(is.na(temp$clay_percent_mean)),] #juste pour check
  table_region <- rbind(table_region, cbind(temp,class_prec = rep(i, nrow(temp))))
  
  mod_rh98 <- readRDS(file = file.path("outputs",
                                       paste0("brms_regression_rh98q90_prec",i,".RDS"))
  )

  temp2 <-  gather_draws(mod_rh98, 
                         c(b_fire_freq_mean,
                           b_precip_mean,
                           b_clay_percent_mean))
                         
  table_post_rh98 <- rbind(table_post_rh98,
                           cbind(temp2,class_prec = as.factor(rep(i, nrow(temp2))))
  )
                          
  mod_cc <- readRDS(file = file.path("outputs",
                                       paste0("brms_regression_ccq90_prec",i,".RDS"))
  )
  temp3 <-  gather_draws(mod_cc, 
                         c(b_fire_freq_mean,
                           b_precip_mean,
                           b_clay_percent_mean))
  
  table_post_cc <- rbind(table_post_cc,
                         cbind(temp3,class_prec = as.factor(rep(i, nrow(temp3))))
  )
}
rm("temp", "temp2","temp3")

world <- ne_countries()
africa <- world %>% filter(continent == "Africa")

## plot posterior distributions
pdf(file.path("figures","one_reg_prec_rh98_post.pdf"), height = 6, width = 6)
ggplot(data = table_post_rh98, 
       aes(x = .value, y = .variable)) +
  stat_halfeye() +
  facet_wrap(.~class_prec)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Value")+
  ylab("Predictor coefficient")+
  ggtitle("RH 98")
dev.off()

pdf(file.path("figures","one_reg_prec_cc_post.pdf"), height = 6, width = 6)
ggplot(data = table_post_cc, 
       aes(x = .value, y = .variable)) +
  stat_halfeye() +
  facet_wrap(.~class_prec)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  xlab("Value")+
  ylab("Predictor coefficient")+
  ggtitle("Canopy cover")
dev.off()

## predictions & plot obs vs pred
table_pred_rh98 <- tibble()
table_pred_cc <- tibble()
for (i in 1:4){
   mod_rh98 <- readRDS(file = file.path("outputs",
                                       paste0("brms_regression_rh98q90_prec",i,".RDS"))
  )
   temp <- data.frame(predict(mod_rh98, ndraws = 50, summary = T, probs = 0.5))
   table_pred_rh98 <- rbind(table_pred_rh98,temp)

    mod_cc <- readRDS(file = file.path("outputs",
                                     paste0("brms_regression_ccq90_prec",i,".RDS"))
  )
    temp2 <- data.frame(predict(mod_cc, ndraws = 50, summary = T, probs = 0.5))
    table_pred_cc <- rbind(table_pred_cc,temp2)
    
}
predictions_rh98 <- cbind(table_region, table_pred_rh98$"Q50")
predictions_cc <- cbind(table_region, table_pred_cc$"Q50")

rm("table_region", "mod_rh98", "mod_cc", "temp", "temp2")

pdf(file.path("figures","one_reg_obs_vs_pred_rh98.pdf"), height = 8, width = 8)
ggplot(data = predictions_rh98, aes(x = rh98_q90, y = table_pred_rh98$Q50)) +
  geom_point(aes(color = fire_freq))+
  scale_color_viridis()+
  facet_wrap(.~class_prec)+
  geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed")+
 # lims(x = c(0,30), y = c(0,30))+
  ggtitle("Q90 Height observed vs predicted")+
  xlab("Observed q90 rh98")+
  ylab("Median predicted q90 rh98")
dev.off()

pdf(file.path("figures","one_reg_obs_vs_pred_cc.pdf"), height = 8, width = 8)
ggplot(data = predictions_cc, aes(x = cc_q90, y = table_pred_cc$Q50)) +
  geom_point(aes(color = fire_freq))+
  scale_color_viridis()+
  facet_wrap(.~class_prec)+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed")+
 # lims(x = c(0,1), y = c(0,1))+
  ggtitle("Q90 Canopy cover observed vs predicted")+
  xlab("Observed q90 canopy cover")+
  ylab("Median predicted q90 canopy cover")
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
