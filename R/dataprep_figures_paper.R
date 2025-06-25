########################################################
###           DATA PREPARATION FOR FIGURES           ###
########################################################

library(dplyr)
library(tidyr)
library(stringr)

#load data
table_region <- readRDS(file.path(
  "transformed_data","subsampled_cell_10e3_for_one_reglin.RDS"))
table_region <- table_region[-which(is.na(table_region$clay_percent_mean)),] #une ligne
values_for_std <- readRDS(file ="rawdata_post_preprocessing/values_for_std.RDS")

#data prep for Fig1
#quantiles of enviro covariables
quantile_enviro <- summary(table_region[,c(14,15,22)])[c(2,3,5),]

cat_clay <- ifelse(table_region$clay_percent < summary(table_region$clay_percent)[2],1,
                   ifelse(table_region$clay_percent >= summary(table_region$clay_percent)[2] &
                            table_region$clay_percent < summary(table_region$clay_percent)[3],2,
                          ifelse(table_region$clay_percent >= summary(table_region$clay_percent)[3] &
                                   table_region$clay_percent < summary(table_region$clay_percent)[5],3,4
                                 )))
cat_prec <- ifelse(table_region$mean_precip < summary(table_region$mean_precip)[2],1,
                   ifelse(table_region$mean_precip >= summary(table_region$mean_precip)[2] &
                            table_region$mean_precip < summary(table_region$mean_precip)[3],2,
                          ifelse(table_region$mean_precip >= summary(table_region$mean_precip)[3] &
                                   table_region$mean_precip < summary(table_region$mean_precip)[5],3,4
                          ))) 
cat_fire <- ifelse(table_region$fire_freq < summary(table_region$fire_freq)[2],1,
                   ifelse(table_region$fire_freq >= summary(table_region$fire_freq)[2] &
                            table_region$fire_freq < summary(table_region$fire_freq)[3],2,
                          ifelse(table_region$fire_freq >= summary(table_region$fire_freq)[3] &
                                   table_region$fire_freq < summary(table_region$fire_freq)[5],3,4
                          )))

df_temp <- cbind(table_region[,3:6], cat_fire=cat_fire, cat_prec=cat_prec, cat_clay=cat_clay)
 
# # back standardization (if we keep enviro values)
# for (i in 1:3){
#   df_temp[,(i+4)] <- df_temp[,(i+4)]*values_for_std[2,i] + values_for_std[1,i]
# }

df_rh98 <- df_temp[,-c(2,4)] 
df_rh98_1 <- df_rh98 %>%
  pivot_longer(cols = starts_with("rh98"), names_to = "quantile", 
               values_to = c("value")) %>%
  mutate(quantile = str_remove(quantile, "rh98_")) %>%
  bind_cols(gedi_var = rep("Height", nrow(df_rh98)*2)) %>%
  relocate(gedi_var, .before = cat_fire) %>%
  relocate(quantile, .before = cat_fire)  %>%
  relocate(value, .before = cat_fire) %>%
  pivot_longer(cols = starts_with("cat_"), names_to = "enviro", 
               values_to = c("category")) %>%
  mutate(enviro = str_remove(enviro, "cat_"))

df_cc <- df_temp[,-c(1,3)] 
df_cc_1 <- df_cc %>%
  pivot_longer(cols = starts_with("cc"), names_to = "quantile", 
               values_to = c("value")) %>%
  mutate(quantile = str_remove(quantile, "cc_")) %>%
  bind_cols(gedi_var = rep("Canopy Cover", nrow(df_cc)*2)) %>%
  relocate(gedi_var, .before = cat_fire) %>%
  relocate(quantile, .before = cat_fire)  %>%
  relocate(value, .before = cat_fire) %>%
  pivot_longer(cols = starts_with("cat_"), names_to = "enviro", 
               values_to = c("category")) %>%
  mutate(enviro = str_remove(enviro, "cat_"))

df_fig1 <- rbind(df_rh98_1, df_cc_1)

saveRDS(df_fig1, file="outputs/df_fig1.RDS")
saveRDS(quantile_enviro, file="rawdata_post_preprocessing/quantile_enviro.RDS")
