library(terra)

#1) load table
complete_table <- readRDS(file = file.path(
  "rawdata_post_preprocessing",
  "6_ecoregions_without_duplicate_standardized_ONLY_over_6_ecoregions.RDS"
)
)

#2) remove fire_frq < 1/20
complete_table_1 <- subset(complete_table, fire_freq > 1/20)
rm(complete_table)

#3) get soil info & intersect & standardized
soil_db <- rast(file.path("rawdata","soil_af_isda",
                          "isda_clay.tot.psa_0-20cm_v0.13_30s.tif"))
test <- terra::extract(soil_db, complete_table_1[,1:2])
rm(soil_db)
colnames(test)[2] <- "clay_percent"
test_std <- (test$clay_percent - mean(test$clay_percent, na.rm = T))/sd(test$clay_percent, na.rm = T)
complete_table_1 <- data.frame(complete_table_1, clay_percent = test$clay_percent,
                               clay_percent_std = test_std)
rm(test)
rm(test_std)

#4) intersect to remove loc where > 10 hab/km2
pop_data <- rast("rawdata/AFR_PPP_2000_adj_v2.tif")
test <- terra::extract(pop_data, complete_table_1[,1:2])
rm(pop_data)
colnames(test)[2] <- "pop_density"
complete_table_1 <- data.frame(complete_table_1, pop_density = test$pop_density)
rm(test)
specific_table <- subset(complete_table_1, pop_density <= 10)
rm(complete_table_1)

#5) cleaning and renaming columns
rownames(specific_table) = 1:nrow(specific_table)
specific_table <- cbind(
  1:nrow(specific_table),
  specific_table$x,
  specific_table$y,
  specific_table
)
colnames(specific_table)[1] = "index"
# rename "x" and "y" columns can be modified as x_TRUE and y_TRUE are saved
colnames(specific_table)[2] = "x_TRUE"
colnames(specific_table)[3] = "y_TRUE"
# predictors and variables
colnames(specific_table)[15] = "prec_std"
colnames(specific_table)[16] = "temp_std"
specific_table <- specific_table[,-14]

saveRDS(
  object = specific_table,
  file = file.path(
    "transformed_data",
    paste0("data_pre_subsampling.RDS")
  )
) 

##version avec cc!= 0 & classes de precip
#rm cc == 0
specific_table1 <- subset(specific_table, canopy_cover !=0)
#classe de precip
classes_prec <- quantile(specific_table1$mean_precip, probs=c(.25,.5,.75))
data_classe1 <- subset(specific_table1, mean_precip <= classes_prec[1])
data_classe2 <- subset(specific_table1, mean_precip > classes_prec[1] & mean_precip <= classes_prec[2])
data_classe3 <- subset(specific_table1, mean_precip > classes_prec[2] & mean_precip <= classes_prec[3])
data_classe4 <- subset(specific_table1, mean_precip > classes_prec[3])

saveRDS(data_classe1, file.path("transformed_data","data_classe_precip1.RDS"))
saveRDS(data_classe2, file.path("transformed_data","data_classe_precip2.RDS"))
saveRDS(data_classe3, file.path("transformed_data","data_classe_precip3.RDS"))
saveRDS(data_classe4, file.path("transformed_data","data_classe_precip4.RDS"))

