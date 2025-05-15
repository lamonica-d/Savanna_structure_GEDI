library(brms)
library(posterior)
library(dplyr)
library(tidybayes)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(sf)
library(ttbary)
library(biscale)
library(gridExtra)
library(ggmosaic)

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


df_plot <- tibble()
class_prec <- as.numeric()
for (j in 1:4){
  temp <- readRDS(file.path("outputs", 
                    paste0("df_mapping_cc_prec",j,".RDS")))
  class_prec <- c(class_prec, rep(j, nrow(temp)))
  df_plot <- rbind(df_plot,temp)
}
df_plot <- cbind(df_plot, class_prec = as.factor(class_prec))

# add color-key
df_color <- tibble(df_plot[df_plot$term == "prec_std",]$unique_id,
                   df_plot[df_plot$term == "prec_std",]$coordx,
                   df_plot[df_plot$term == "prec_std",]$coordy,
                   as.factor(df_plot[df_plot$term == "prec_std",]$effect),
                   as.factor(df_plot[df_plot$term == "fire_freq_std",]$effect),
                   as.factor(df_plot[df_plot$term == "clay_percent_std",]$effect),
                   as.factor(df_plot[df_plot$term == "prec_std",]$class_prec)
)

colnames(df_color) <- c("unique_id", "coordx", "coordy",
  "prec_std", "fire_freq_std", "clay_percent_std", "class_prec")

# df_color <- bi_class(df_color, x = "fire_freq_std", y = "clay_percent_std", 
#                       dim = 3)
# 
# (truc <- viridis_pal()(12))
# custom_pal <- c(
#   "1-1" = "#440154", # negative fire, negative clay
#   "2-1" = "#38598C", # no effect fire, negative clay
#   "3-1" = "#85D54A", # positive fire, negative clay
#  
#    "1-2" = "#482173" , # negative fire, no effect clay
#   "2-2" = "#808080", # no effect fire, no effect clay
#   "3-2" = "#C2DF23", # positive fire, no effect clay
#  
#    "1-3" = "#433E85", # negative fire, positive clay
#   "2-3" = "#51C56A", # no effect fire, positive clay
#   "3-3" = "#FDE725"  # positive fire, positive clay
# )

df_plot_density <- df_plot %>%
  subset(term != "prec_std")
var.labs <- c("Clay percent", "Fire frequence")
names(var.labs) <- c("clay_percent_std","fire_freq_std")

df_plot_prec_estim <- df_plot %>%
  subset(term == "prec_std")

## plot
# map background
world <- ne_countries()
africa <- world %>% filter(continent == "Africa")

# map precipitation class
fig_a <- ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_point(data=df_color, mapping = aes(x= coordx, y = coordy,
                                          colour = class_prec), shape = 1)+
  coord_sf(xlim=c(-20, 37), ylim=c(-15, 20), expand=FALSE)+
  scale_colour_viridis_d() +
  labs(colour = "Precipitation classes")+
  theme(panel.background=element_rect(fill="slategray1"), legend.position = "bottom",
        legend.key = element_rect(fill = "white"))+
  ggtitle("(a) Map of cells according to precipitation classes")+
  xlab("long")+
  ylab("lat")
 
# plot estimates
fig_b <- ggplot(data = df_plot_prec_estim) +
  geom_segment(aes(x = class_prec, y = .lower, xend = class_prec, yend = .upper,
                   colour = class_prec))+
  geom_point(aes(x = class_prec, y = r_unique_id, colour = class_prec))+
  theme_minimal()+
  scale_color_viridis_d()+
  ggtitle("(b) Median and 95%CI of precipitation effect")+
  xlab("Precipitation class")+
  ylab("Coefficient estimates")+
  theme(legend.position = "none")

# ggplot(data = df_plot_density) +
#   geom_density(aes(x = r_unique_id, colour = class_prec))+
#   facet_grid(.~ term)+
#   theme_minimal()+
#   scale_color_viridis_d()
fig_c <- ggplot(data = df_plot_density) +
  geom_density(aes(x = r_unique_id, colour = class_prec))+
  facet_grid(.~ term, labeller = labeller(term = var.labs), scales = "free")+
  theme_minimal()+
  scale_color_viridis_d()+
  theme(legend.position = "none")+
  #xlab("Precipitation class")+
  ylab("Median coefficient estimates")+
  ggtitle("(c) Distribution of median values \n of coefficients accross cells")

# map of effects clay percent & fire freq
 fig_d_list <- list()
 for (i in 1:4){
fig_d_list[[i]] <- ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_point(data=df_color[df_color$class_prec == i,], mapping = aes(x= coordx, y = coordy,
                                          colour = fire_freq_std,
                                          shape = clay_percent_std), size=2)+
                    #bi_class)) +
  coord_sf(xlim=c(-20, 37), ylim=c(-15, 20), expand=FALSE)+
  #bi_scale_color(pal = custom_pal, dim = 3) +
    scale_color_manual(values = c("#440154", "grey","#51C56A" ))+
    scale_shape_manual(values=c(0,1,2))+ #+15)+
  #facet_wrap(vars(class_prec))+
  labs(colour = "Fire frequency effect", shape = "Clay percentage effect")+
  theme(panel.background=element_rect(fill="slategray1"), 
        legend.position = "right")+
  ggtitle("(d) Combination of clay percent and fire frequence effects")+
  xlab("long")+
  ylab("lat")
}

#gather subfigures
# plots <- list(fig_a, fig_b, fig_c) #, fig_d)
# lay <- rbind(c(1,1), c(2,3)) #, c(4,4))
#grid.arrange(grobs = plots, layout_matrix = lay)
#grid.arrange(fig_a, fig_b, fig_c, ncol = 1)

pdf(file="fig_a.pdf",width=12, height=6)
fig_a
dev.off()

pdf(file="fig_bc.pdf",width=8, height=4)
grid.arrange(fig_b, fig_c, ncol = 2)
dev.off()

pdf(file="fig_d.pdf",width=12, height=6)
fig_d
dev.off()


## contingency tables
con_all <- xtabs(~clay_percent_std+fire_freq_std, data=df_color)
prop.table(ftable(con_all))

plot_con_table <- list()
for (i in 1:4){
  con_all2 <- df_color %>%
    filter(class_prec == i) %>%
    select(clay_percent_std, fire_freq_std) %>%
    as_tibble()
  con_all2 %>% table()
  
  plot_con_table[[i]] <- con_all2 %>%
    ggplot() +  
    geom_mosaic(aes(x = product(clay_percent_std), fill = fire_freq_std)) +  
    scale_fill_manual(values = c("#440154", "grey","#51C56A" ))+
    theme_mosaic()+
    theme(legend.position = "none")
}
grid.arrange(plot_con_table[[1]], plot_con_table[[2]],
             plot_con_table[[3]], plot_con_table[[4]], ncol = 2)

con_all2 <- df_color %>%
  select(clay_percent_std, fire_freq_std) %>%
  as_tibble()
con_all2 %>% table()

p1 <- con_all2 %>%
  ggplot() +  
  geom_mosaic(aes(x = product(clay_percent_std), fill = fire_freq_std)) +  
  scale_fill_manual(values = c("#440154", "grey","#51C56A" ))+
  theme_mosaic()+
  theme(legend.position = "none")

}
