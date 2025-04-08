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

data <- readRDS(file.path("transformed_data_ilots",
                          "data_point_in_cells_10e4_prec_subsampled.RDS"))

for (j in 1:4){

model <- readRDS(file.path("outputs", paste0("mod_rh98_prec",j,".RDS")))
coeff <- readRDS(file.path("outputs", paste0("coeff_mod_rh98_prec",j,".RDS")))
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
        file = file.path("outputs", paste0("df_mapping_rh98_prec",j,".RDS")))
}


df_plot <- tibble()
class_prec <- as.numeric()
for (j in 1:4){
  temp <- readRDS(file.path("outputs", 
                    paste0("df_mapping_rh98_prec",j,".RDS")))
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

df_color <- bi_class(df_color, x = "fire_freq_std", y = "clay_percent_std", 
                      dim = 3)

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
  geom_boxplot(aes(x = class_prec,y = r_unique_id, fill = class_prec))+
  facet_grid(.~ term, labeller = labeller(term = var.labs), scales = "free")+
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(legend.position = "none")+
  xlab("Precipitation class")+
  ylab("Median coefficient estimates")+
  ggtitle("(c) Distribution of median values \n of coefficients accross cells")

# map of effects clay percent & fire freq
fig_d <- ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_point(data=df_color, mapping = aes(x= coordx, y = coordy,
                                          colour = bi_class)) +
  coord_sf(xlim=c(-20, 37), ylim=c(-15, 20), expand=FALSE)+
  bi_scale_color(pal = "BlueYl", dim = 3) +
  labs(colour = "Variable effect combinations")+
  theme(panel.background=element_rect(fill="slategray1"), 
        legend.position = "right")+
  ggtitle("(d) Combination of clay percent and fire frequence effects")+
  xlab("long")+
  ylab("lat")

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
