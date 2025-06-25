##################################################
###                      FIGURES               ###
##################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(brms)
library(tidybayes)
library(rnaturalearth)

#FIGURE 1
##load data
df_fig1 <- readRDS(file="outputs/df_fig1.RDS")
quantile_enviro <- readRDS(file="rawdata_post_preprocessing/quantile_enviro.RDS")

##plot
fig1 <- ggplot(data=df_fig1[df_fig1$quantile == "q90",])+
     geom_boxplot(aes(x = category, y = value, group = category), fill = "gray")+
     scale_x_continuous(breaks=c(1.5,2.5,3.5), limits=c(0.6, 4.4),
                                labels=c("25%", "50%", "75%"))+
  facet_wrap(gedi_var~enviro, scales = "free", ncol = 3,
             labeller = as_labeller(c("Height" = "Tree height (m)", "Canopy Cover" = "Canopy cover (%)", "clay" = "Clay percentage", "fire" = "Fire frequency", "prec" = "Mean annual \n precipitation (mm)")))+
  theme_minimal()+
  ggtitle("GEDI variables according to environment covariable")+
  xlab("Quantile of environmental covariable")+
  ylab("90% quantile of GEDI variables")

pdf(file=file.path("figures","paper_fig_1.pdf"),width=6, height=6)
fig1
dev.off()

name_enviro <- unique(df_fig1$enviro)
xlab_vect <- c("Fire frequency", "Mean annual precipitation (mm)","Clay percentage")
plot_list <- list()
for (i in 1:3){
if (i==1) {plot_list[[i]] <- ggplot(data=df_fig1[df_fig1$quantile == "q90" & df_fig1$enviro == name_enviro[i],])+
  geom_boxplot(aes(x = category, y = value, group = category), fill = "gray")+
  scale_x_continuous(breaks=c(1.5,2.5,3.5), limits=c(0.6, 4.4),
                     labels=quantile_enviro[,i])+
  facet_wrap(gedi_var~., scales = "free", ncol = 1,
             labeller = as_labeller(c("Height" = "Tree height (m)", "Canopy Cover" = "Canopy cover (%)", "clay" = "Clay percentage", "fire" = "Fire frequency", "prec" = "Mean annual precipitation (mm)")))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=20))+
  ggtitle("GEDI variables according to environment covariable")+
  xlab(xlab_vect[i])+
  ylab("90% quantile of GEDI variables")

}else{
 
    plot_list[[i]] <- ggplot(data=df_fig1[df_fig1$quantile == "q90" & df_fig1$enviro == name_enviro[i],])+
    geom_boxplot(aes(x = category, y = value, group = category), fill = "gray")+
    scale_x_continuous(breaks=c(1.5,2.5,3.5), limits=c(0.6, 4.4),
                       labels=quantile_enviro[,i])+
    facet_wrap(gedi_var~., scales = "free", ncol = 1,
               labeller = as_labeller(c("Height" = "Tree height (m)", "Canopy Cover" = "Canopy cover (%)", "clay" = "Clay percentage", "fire" = "Fire frequency", "prec" = "Mean annual precipitation (mm)")))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=20))+
    ggtitle("")+
    xlab(xlab_vect[i])+
    ylab("")}
}

pdf(file=file.path("figures","paper_fig_1_v2.pdf"),width=12, height=6)
grid.arrange(plot_list[[1]],plot_list[[2]],plot_list[[3]], ncol=3)
dev.off()


#FIGURE 2
##load data

mod_rh98 <- readRDS(file = "outputs/brms_regression_rh98q90_22_05.RDS")
mod_canopy_cover <- readRDS(file = "outputs/brms_regression_ccq90_22_05.RDS")

##prep data
table_post_rh98 <- mod_rh98 %>%
  gather_draws(c(b_fire_freq_mean,b_precip_mean,b_clay_percent_mean)) %>%
  median_qi(.width = 0.99) %>%
  bind_cols(gedi_var = rep("Tree height", 3))
table_post_cc <- mod_canopy_cover %>%
  gather_draws(c(b_fire_freq_mean,b_precip_mean,b_clay_percent_mean)) %>%
  median_qi(.width = 0.99) %>%
  bind_cols(gedi_var = rep("Canopy cover", 3))
  
table_post <- rbind(table_post_rh98, table_post_cc)

##plot
pdf(file.path("figures","paper_fig_2.pdf"), height = 5, width = 10)
 ggplot(table_post) +
  geom_pointinterval(aes(y = .value, x = .variable, ymin = .lower, ymax = .upper),
                     size = 0, linewidth = 3) +
  facet_wrap(.~ gedi_var, scales = "free", ncol =1)+
   theme_minimal()+
   scale_x_discrete(labels=c("Clay", "Fire","Precipitation"))+
  ylab("Value")+
  xlab("Predictor")+
  ggtitle("Posterior distributions (median and 95% interval) of regression coefficients")+
   geom_hline(yintercept = 0, color="darkgrey", linetype = "dashed")+
   coord_flip()
dev.off()

#FIGURE 3
##load data
world <- ne_countries()
africa <- world %>% filter(continent == "Africa")
gedi_var <- c("rh98", "cc")
gedi_var_names <- c("Tree height", "Canopy cover")
data_fig4 <- readRDS(file = "outputs/data_paper_fig3.RDS")

for (v in 1:2){
df_var <- data_fig4[[v]] %>%
  pivot_longer(cols = ends_with("_std"), names_to = "enviro", 
                values_to = c("effect_direction")) %>%
  filter(enviro != "prec_std")
df_var$effect_direction <- factor(df_var$effect_direction, 
                                  levels = c("-1", "0", "1"))
  
fig <- ggplot(data=africa) +
  geom_sf(color="black") + 
  geom_point(data=df_var, mapping = aes(x= coordx, y = coordy,
                                        colour = effect_direction), size=2, shape=1)+
  coord_sf(xlim=c(-20, 37), ylim=c(-15, 20), expand=FALSE)+
  scale_color_manual(values = c("#440154", "grey","#51C56A" ))+
  facet_wrap(enviro~class_prec, ncol =4,
             labeller = as_labeller(c("1" = "Precipitation class 1","2" = "Precipitation class 2",
                                      "3" = "Precipitation class 3","4" = "Precipitation class 4",
               "clay_percent_std" = "Clay percentage", "fire_freq_std" = "Fire frequency")))+
  labs(colour = "Direction of effect") + 
  theme(panel.background=element_rect(fill="slategray1"), 
        legend.position = "bottom")+
  ggtitle(paste0(gedi_var_names[v], ", clay and fire effects per precipitation class"))+
  xlab("long")+
  ylab("lat")


pdf(file=file.path("figures",paste0("paper_fig_3_", gedi_var[v],".pdf")),width=12, height=6)
print(fig)
dev.off()


}

