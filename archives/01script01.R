#raw data visualisation

# For the accents é ù û etc
options(Encoding="latin1")
# Cleaning the environment
rm(list=ls())
# Getting the paths
source("paths.R",encoding="latin1")
# Setting the current path
path_to_R_folder = file.path(
  path_to_Savanna_structure_GEDI_folder,
  "R"
)
setwd(path_to_R_folder)
getwd()

# Libraries
library(fst)
library(ggplot2)

###########
setwd(path_to_GEDI_raw_data)

data_north_congo <- fst::read.fst("Northern_Congolian.fst")
data_guinee <- fst::read.fst("Guinean_forest-savanna.fst")

par(mfrow= c(2,2))
plot(data_north_congo$fire_freq, data_north_congo$rh98, las = 1)
plot(data_north_congo$fire_freq, data_north_congo$canopy_cover, las = 1)
plot(data_north_congo$mean_precip, data_north_congo$rh98, las = 1)
plot(data_north_congo$mean_precip, data_north_congo$canopy_cover, las = 1)

par(mfrow= c(2,2))
plot(data_guinee$fire_freq[1:400], data_guinee$rh98[1:400], las = 1)
plot(data_guinee$fire_freq[1:400], data_guinee$canopy_cover[1:400], las = 1)
plot(data_guinee$mean_precip[1:400], data_guinee$rh98[1:400], las = 1)
plot(data_guinee$mean_precip[1:400], data_guinee$canopy_cover[1:400], las = 1)


