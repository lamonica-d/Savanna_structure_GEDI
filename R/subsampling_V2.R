### prépa des données pour nouveau test du modèle

#1) load table
complete_table <- readRDS(file = file.path(
    "rawdata_post_preprocessing",
    "6_ecoregions_without_duplicate_standardized_ONLY_over_6_ecoregions.RDS"
  )
)

#2) remove fire_frq < 1/20
complete_table_1 <- subset(complete_table, fire_freq > 1/20)
rm(complete_table)

#3) intersect to remove loc where > 10 hab/km2
