library(brms)
library(posterior)
library(dplyr)
library(tidybayes)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(sf)
library(ttbary)
library(gridExtra)
library(ggmosaic)

data <- readRDS(file.path("transformed_data_ilots",
                          "data_point_in_cells_10e4_prec_subsampled.RDS"))

gedi_var <- c("rh98", "cc")
gedi_var_names <- c("Tree height", "Canopy cover")

# map background
world <- ne_countries()
africa <- world %>% filter(continent == "Africa")

# load data
df_plot_list <- list()
df_color_list <- list()

for (v in 1:2){
  
df_plot <- tibble()
class_prec <- as.numeric()

for (j in 1:4){
  temp <- readRDS(file.path("outputs", 
                            paste0("df_mapping_",gedi_var[v],"_prec",j,".RDS")))
  class_prec <- c(class_prec, rep(j, nrow(temp)))
  df_plot <- rbind(df_plot,temp)
}
df_plot <- cbind(df_plot, class_prec = as.factor(class_prec))

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

df_plot_list[[v]] <- df_plot
df_color_list[[v]] <- df_color
}
rm(df_plot)
rm(df_color)

# Figure 1 points per class precip

fig1 <- ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_point(data=df_color_list[[1]], mapping = aes(x= coordx, y = coordy,
                                          colour = class_prec), shape = 1)+
  coord_sf(xlim=c(-20, 37), ylim=c(-15, 20), expand=FALSE)+
  scale_colour_viridis_d() +
  labs(colour = "Precipitation classes")+
  theme(panel.background=element_rect(fill="slategray1"), legend.position = "bottom",
        legend.key = element_rect(fill = "white"))+
  ggtitle("(1) Map of cells according to precipitation classes")+
  xlab("long")+
  ylab("lat")

pdf(file=file.path("figures","fig_1.pdf"),width=12, height=6)
fig1
dev.off()
   
# Figure 2 plot coeff precip & fire + clay

# plot estimates

for (v in 1:2){
  
df_plot <- df_plot_list[[v]]

df_plot_density <- df_plot %>%
  subset(term != "prec_std")
var.labs <- c("Clay percent", "Fire frequence")
names(var.labs) <- c("clay_percent_std","fire_freq_std")

df_plot_prec_estim <- df_plot %>%
  subset(term == "prec_std")

fig_b <- ggplot(data = df_plot_prec_estim) +
  geom_segment(aes(x = class_prec, y = .lower, xend = class_prec, yend = .upper,
                   colour = class_prec))+
  geom_point(aes(x = class_prec, y = r_unique_id, colour = class_prec))+
  theme_minimal()+
  scale_color_viridis_d()+
  ggtitle("(2b) Median and 95%CI of precipitation effect")+
  xlab("Precipitation class")+
  ylab("Coefficient estimates")+
  theme(legend.position = "none")

fig_c <- ggplot(data = df_plot_density) +
  geom_density(aes(x = r_unique_id, colour = class_prec))+
  facet_grid(.~ term, labeller = labeller(term = var.labs), scales = "free")+
  theme_minimal()+
  scale_color_viridis_d()+
  theme(legend.position = "none")+
  xlab("")+
  ylab("Median coefficient estimates")+
  ggtitle("(2c) Distribution of median values \n of coefficients accross cells")

pdf(file=file.path("figures",paste0("fig_bc_", gedi_var[v],".pdf")),width=8, height=4)
grid.arrange(fig_b, fig_c, ncol = 2)
dev.off()

}

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
