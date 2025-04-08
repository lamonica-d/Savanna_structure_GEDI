
library(fst)

#raw data directory
dir_rawdata <- "~/Documents/dessfor/gedi_data_lebien/pix_extract"

# Ecoregions :
list.files(path = dir_rawdata ,full.names=FALSE)
# 6 selected ecoregions
list.files(path = dir_rawdata,full.names=FALSE)[c(10,18,19,23,26,27)]

# Concatenation of all data in complete_corresponding_table

complete_corresponding_table = data.frame(matrix(nrow=0,ncol=9))

colnames(complete_corresponding_table) <-  c("x",
                                             "y",
                                             "rh98",
                                             "canopy_cover",
                                             "fire_freq",
                                             "mean_precip",
                                             "mean_temp",
                                             "ecoregion",
                                             "fire_freq_NA")

for (i in c(10,18,19,23,26,27)){

  name = dir(dir_rawdata)[i]
  print(name)
  corresponding_table = fst::read.fst(file.path(dir_rawdata,name))
  
  # Let's only keep the following columns :
  
  corresponding_table <- corresponding_table[ ,c("x",
                                                 "y",
                                                 "rh98",
                                                 "canopy_cover",
                                                 "fire_freq",
                                                 "mean_precip",
                                                 "mean_temp",
                                                 "ecoregion"
  )]
  
  # Let's get rid of the ".fst" in the graph name
  name <- substr(name, start = 1, stop =  nchar(name) - 4)
  print(name)
  
  # Creation of a "fire_freq_NA" column and NA fire_freq replacement by 0s :
  
  TRUE_FALSE_is_fire_freq_NA <- is.na(corresponding_table[,"fire_freq"])
  corresponding_table["fire_freq_NA"] <- TRUE_FALSE_is_fire_freq_NA
  corresponding_table[which(TRUE_FALSE_is_fire_freq_NA),"fire_freq"] <- 0
  
  # Let's keep only the complete rows :
  # (45 millions of rows if we do this, 48 without this)
  
  corresponding_table <- corresponding_table[
    complete.cases(
      corresponding_table[,c("x",
                             "y",
                             "rh98",
                             "canopy_cover",
                             "fire_freq",
                             "mean_precip",
                             "mean_temp",
                             "ecoregion",
                             "fire_freq_NA"
      )]
    ),]
  
  
  # Concatenation of corresponding_table with the previous ones :
  
  complete_corresponding_table = rbind(complete_corresponding_table,corresponding_table)
  
}

# same as the previous line but keeping other columns :
require(dplyr)
without_duplicate <- complete_corresponding_table %>% distinct(x, y, .keep_all = TRUE)

saveRDS(
  object = without_duplicate,
  file = file.path("rawdata_post_preprocessing",
    "complete_corresponding_table_without_duplicate.RDS"
  )
)


require(janitor)
duplicates <- janitor::get_dupes(without_duplicate, "x", "y") # prend quelques minutes
# On doit avoir le message :
# -> No duplicate combinations found of: x, y

# Standardisation et sauvegarde des données uniquement sur :

list.files(path = dir_rawdata,full.names=FALSE)[c(10,18,19,23,26,27)]

names = c("Guinean_forest-savanna","West_Sudanian_savanna","Sahelian_Acacia_savanna",
          "Northern_Congolian_Forest-Savanna","Western_Congolian_forest-savanna","Southern_Congolian_forest-savanna")

sub_table <- readRDS(file = file.path(
  "rawdata_post_preprocessing",
  "complete_corresponding_table_without_duplicate.RDS"
)
)

nrow(sub_table)
sub_table <- sub_table[which(sub_table[,"ecoregion"] %in% names), ]
nrow(sub_table)

unique(sub_table$ecoregion)

sub_table = cbind(sub_table,as.vector(scale(sub_table$fire_freq, center = TRUE, scale = TRUE)))
sub_table = cbind(sub_table,as.vector(scale(sub_table$mean_precip, center = TRUE, scale = TRUE)))
sub_table = cbind(sub_table,as.vector(scale(sub_table$mean_temp, center = TRUE, scale = TRUE)))

colnames(sub_table)
head(sub_table)
ncol(sub_table)

colnames(sub_table)[10] = "fire_freq_std"
colnames(sub_table)[11] = "mean_precip_std"
colnames(sub_table)[12] = "mean_temp_std"

colnames(sub_table)

summary(sub_table)
colnames(sub_table)

saveRDS(
  object = sub_table,
  file = file.path("rawdata_post_preprocessing",
    "6_ecoregions_without_duplicate_standardized_ONLY_over_6_ecoregions.RDS"
  )
)
