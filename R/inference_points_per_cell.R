##inference points per cell
library(brms)
library(posterior)

data <- readRDS(file.path("transformed_data_ilots","data_point_in_cells_10e4.RDS"))

for (j in 1:length(data)){
  temp <- data[[j]]
  cell_id_vect <- unique(temp$cell_id)
  
  print(j)
  print(cell_id_vect)
  
  outputs_model_rh98 <- list()
  #outputs_model_cc <- list()
  
  for (i in 1:length(cell_id_vect)){ 
   cell_select <- temp[which(temp$cell_id==cell_id_vect[i]),]
   print(i)
   print(nrow(cell_select))
  if (nrow(cell_select) > 20){
   
   mod_rh98 <- brm(
     rh98 ~ fire_freq_std + prec_std + clay_percent_std,
     data = cell_select,family = brmsfamily(family = "Gamma",link="log"),
     prior = NULL,warmup = 5*10**2,iter = 5*10**3,thin = 10,
     chains = 3,cores = 3,silent=2
     )
   
   # mod_cc <- brm(
   #   cc ~ fire_freq_std + prec_std + clay_percent_std,
   #   data = cell_select,
   #   family = brmsfamily(family = "zero_inflated_beta",link = "logit",
   #                       link_phi = "log",link_zi = "logit"),
   #   prior = NULL,warmup = 5*10**2,iter = 5*10**3,thin = 10,
   #   chains = 3,cores = 3,silent=2
   # )
   
   outputs_model_rh98[[i]] <- summarise_draws(mod_rh98)[,-c(5,10)]
   rm(mod_rh98)
   # outputs_model_cc[[i]] <- mod_cc
  }else{next}
  
   }
  
saveRDS(outputs_model_rh98, file = 
          file.path("outputs", paste0("list_outputs_model_rh98_j",j,".RDS")))
saveRDS(outputs_model_cc, file = 
          file.path("outputs", paste0("list_outputs_model_cc_j",j,".RDS"))) 

rm(outputs_model_cc)
rm(outputs_model_rh98)
}


## analyse rapide
library(dplyr)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(sf)

readRDS(file.path("outputs","list_outputs_model_rh98_j2.RDS")) -> output
data <- readRDS(file.path("transformed_data_ilots","data_point_in_cells_10e4.RDS"))

# extent_list <- readRDS(file.path("outputs","extent_list_10e5.RDS"))
# nb_cpu <- 20
# extent_list_parallel <- lapply(splitIndices(length(extent_list),nb_cpu),
#                                function(i) extent_list[i])

j=2
#extent <- extent_list_parallel[[j]]
temp <- data[[j]]
cell_id_vect <- unique(temp$cell_id)
rm(data)
#rm(extent_list)

b_fire_q50 <- as.numeric()  
b_prec_q50 <- as.numeric()  
b_clay_q50 <- as.numeric()  
xplot <- as.numeric()  
yplot <- as.numeric()  

for (i in 1:length(output)){
  cell_select <- temp[which(temp$cell_id==cell_id_vect[i]),]
  output_select <- output[[i]]
  
  if (nrow(cell_select) > 20){
  b_fire_q50[i] <- output_select$median[2]
  b_prec_q50[i] <- output_select$median[3]
  b_clay_q50[i] <- output_select$median[4]
  xplot[i] <- cell_select$coordxTRUE[1]
  yplot[i] <- cell_select$coordyTRUE[1]
  
  }else{
    b_fire_q50[i] <- NA
    b_prec_q50[i] <- NA
    b_clay_q50[i] <- NA
    xplot[i] <- NA
    yplot[i] <- NA
  }
}

df_plot <- data.frame(xplot = xplot, yplot = yplot, b_fire_q50 = b_fire_q50
                      , b_prec_q50 = b_prec_q50, b_clay_q50 = b_clay_q50)
df_plot2 <- subset(df_plot, is.na(xplot)==F)
plot_sf <- st_as_sf(df_plot2, coords = c("xplot", "yplot"),
                     crs = "+proj=longlat +datum=WGS84")
#plot
##fond
world <- ne_countries()
africa <- world %>% filter(continent == "Africa")


#fire
df_plot_fire <- subset(df_plot2, b_fire_q50>-100&b_fire_q50<100)
plot_sf_fire <- st_as_sf(df_plot_fire, coords = c("xplot", "yplot"),
         crs = "+proj=longlat +datum=WGS84")
ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_sf(data=plot_sf_fire, aes(color = b_fire_q50))+
  scale_color_viridis()+
  coord_sf(xlim=c(-20, 6), ylim=c(14, 18), expand=FALSE)+
  labs(color = "Medianlinear coefficient")+
  ggtitle("Fire effect on RH98")+
  theme(panel.background=element_rect(fill="slategray1"))

ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_sf(data=plot_sf, aes(color = b_clay_q50))+
  scale_color_viridis()+
  coord_sf(xlim=c(-20, 6), ylim=c(14, 18), expand=FALSE)+
  labs(color = "Median linear coefficient")+
  ggtitle("Clay percent effect on RH98")+
  theme(panel.background=element_rect(fill="slategray1"))

df_plot_prec <- subset(df_plot2, b_prec_q50>-100&b_prec_q50<100)
plot_sf_prec <- st_as_sf(df_plot_prec, coords = c("xplot", "yplot"),
                         crs = "+proj=longlat +datum=WGS84")
ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_sf(data=plot_sf_prec, aes(color = b_prec_q50))+
  scale_color_viridis()+
  coord_sf(xlim=c(-20, 6), ylim=c(14, 18), expand=FALSE)+
  labs(color = "Median linear coefficient")+
  ggtitle("Precipitation effect on RH98")+
  theme(panel.background=element_rect(fill="slategray1"))
