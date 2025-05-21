library(brms)
library(posterior)
library(dplyr)
library(tidybayes)
library(sf)
library(ttbary)

data <- readRDS(file.path("transformed_data_ilots",
                          "data_point_in_cells_10e4_prec_subsampled.RDS"))

gedi_var <- c("rh98", "cc")

for (v in 1:2){
  var_int <- gedi_var[v]

for (j in 1:4){

model <- readRDS(file.path("outputs", paste0("mod_", var_int, "_prec",j,".RDS")))
coeff <- readRDS(file.path("outputs", paste0("coeff_mod_", var_int, "_prec",j,".RDS")))
data_prec <- data[[j]]

## get prec coefficient
coeff_prec <- summarise_draws(model)[2,c(3,6:7)]

## coord cells
coord_cell <- tibble(.rows = length(unique(data_prec$unique_id)))
for (i in 1:length(unique(data_prec$unique_id))){
temp <- data_prec %>%
  filter(unique_id == unique(data_prec$unique_id)[i])
  
coord_cell <- rbind(coord_cell,
                    as.vector(drezner(temp$coordxTRUE, temp$coordyTRUE, penalty = 2)[1:2]))
}
coord_cell <- cbind(unique_id = unique(data_prec$unique_id), coord_cell)
colnames(coord_cell) <- c("unique_id", "coordx", "coordy") 

rm(data_prec)

## table for plotting
df_plot <- coeff[,1:5] %>%
  mutate(term = replace(term, term == "Intercept", "prec_std")) %>%
  mutate(r_unique_id = replace(r_unique_id, term == "prec_std", coeff_prec$median)) %>%
  mutate(.lower = replace(.lower, term == "prec_std", coeff_prec$q5)) %>%
  mutate(.upper = replace(.upper, term == "prec_std", coeff_prec$q95)) %>%
  full_join(coord_cell, by = "unique_id") %>%
  mutate(effect = case_when(.lower < 0 & .upper > 0 ~ 0,
                            .lower > 0 ~ 1,
                            .upper < 0 ~ -1))
saveRDS(df_plot, 
        file = file.path("outputs", paste0("df_mapping_cc_prec",j,".RDS")))
}
}

