# Cleaning the environment
rm(list=ls())
# Getting the paths
getwd()
source("paths.R")

save_png = TRUE

full_table <- readRDS(file.path(path_to_Savanna_structure_GEDI_folder,"rawdata_post_preprocessing","complete_corresponding_table_without_duplicate.RDS"))

ecoregions = c("Guinean_forest-savanna","West_Sudanian_savanna","Sahelian_Acacia_savanna",
               "Northern_Congolian_Forest-Savanna","Western_Congolian_forest-savanna","Southern_Congolian_forest-savanna")

# for tests :
# ecoregions = c("Guinean_forest-savanna")

set.seed(0)    
for(ecoregion in ecoregions){
  table <- full_table[full_table[,"ecoregion"]==ecoregion,]
  # table <- full_table # for a sample above all ecoregions
  # ecoregion = "all" # for a sample above all ecoregions
  
  rownames(table) <- 1:nrow(table)
  print(paste(ecoregion,nrow(table)))
  # table <- table[sample(1:nrow(table),size=min(5*10**3,nrow(table)),replace=FALSE),
  #                c("rh98","canopy_cover","fire_freq","mean_precip","mean_temp")]
  # 
  # if(save_png==TRUE){
  # png(
  # filename = file.path(
  #                       path_to_Savanna_structure_GEDI_folder,
  #                       "figures",
  #                       "scatterplots",
  #                       paste0("scatterplot_",ecoregion,".png")
  #                       ),
  # width = 10**3, height = 10**3
  # )
  # pairs(table,main = ecoregion)
  # dev.off()
  # }
  
}  

############## hist

require("ggplot2")
require("gridExtra")
require("grid")

for(ecregion in ecoregions){
  
  corresponding_table <- full_table[full_table[,"ecoregion"]==ecoregion,]
  
  print(paste(name,nrow(table_region)))
  
if(
  length(corresponding_table[,"canopy_cover"])>0 # s'il y a des donn?es
){
  
  plot_cc <- ggplot(
    corresponding_table,
    aes(x=canopy_cover)
  ) +
    geom_histogram(
      position="identity",
      bins = 30,
      colour = "white",
      fill = "forestgreen",
      alpha = 0.6
    ) +
    labs(x = paste0("canopy_cover"
    )
    )
}
else{
  print("NO DATA")
}

if(
  length(corresponding_table[,"fire_freq"])>0 # s'il y a des donn?es
){
  plot_fire <- ggplot(
    corresponding_table,
    aes(x=fire_freq)
  ) +
    geom_histogram(
      position="identity",
      bins = 30,
      colour = "white",
      fill = "red",
      alpha = 0.6
    ) +
    labs(x = paste0("fire_freq"
    )
    )
}
else{
  print("NO DATA")
}

if(
  length(corresponding_table[,"mean_precip"])>0 # s'il y a des données
){
  plot_rain <- ggplot(
    corresponding_table,
    aes(x=mean_precip)
  ) +
    geom_histogram(
      position="identity",
      bins = 30,
      colour = "white",
      fill = "skyblue",
      alpha = 0.6
    ) +
    labs(x = paste0("mean_precip"
    )
    )
}

else{ print("NO DATA") }
 
if(
  length(corresponding_table[,"rh98"])>0 # s'il y a des donn?es
){
  plot_rh98 <- ggplot(
    corresponding_table,
    aes(x=rh98)
  ) +
    geom_histogram(
      position="identity",
      bins = 30,
      colour = "white",
      fill = "limegreen",
      alpha = 0.6
    ) +
    labs(x = paste0("rh98"
    )
    )
}
else{ print("NO DATA") }

plot <- grid.arrange(
  plot_cc, plot_rh98, plot_fire, plot_rain,
  nrow = 2, ncol=2,
  bottom = textGrob(ecoregion,gp=gpar(fontsize=20,font=1))
)

# ggsave(
#        file.path(path_to_Savanna_structure_GEDI_folder,"figures","hist_13Mai",
#                  paste0("hist_full_",ecoregion,".png")),
#        plot = plot,
#        scale = 2, # au pif total du premier coup et ça marche :D
#        bg='#ffffff'# pour ne pas avoir un fond gris moche
#        )

}

### hist post subsampling

require("ggplot2")
require("gridExtra")
require("grid")
for(ecoregion in ecoregions){
  
  corresponding_table <- readRDS(
    file.path(
      path_to_Savanna_structure_GEDI_folder,
      "transformed_data",
      paste0(ecoregion,".RDS"))
  )
  
  print(paste(name,nrow(corresponding_table)))
  
  if(
    length(corresponding_table[,"canopy_cover"])>0 # s'il y a des donn?es
  ){
    
    plot_cc <- ggplot(
      corresponding_table,
      aes(x=canopy_cover)
    ) +
      geom_histogram(
        position="identity",
        bins = 30,
        colour = "white",
        fill = "forestgreen",
        alpha = 0.6
      ) +
      labs(x = paste0("canopy_cover"
      )
      )
  }
  else{
    print("NO DATA")
  }
  
  if(
    length(corresponding_table[,"fire_freq"])>0 # s'il y a des donn?es
  ){
    plot_fire <- ggplot(
      corresponding_table,
      aes(x=fire_freq)
    ) +
      geom_histogram(
        position="identity",
        bins = 30,
        colour = "white",
        fill = "red",
        alpha = 0.6
      ) +
      labs(x = paste0("fire_freq"
      )
      )
  }
  else{
    print("NO DATA")
  }
  
  if(
    length(corresponding_table[,"mean_precip"])>0 # s'il y a des données
  ){
    plot_rain <- ggplot(
      corresponding_table,
      aes(x=mean_precip)
    ) +
      geom_histogram(
        position="identity",
        bins = 30,
        colour = "white",
        fill = "skyblue",
        alpha = 0.6
      ) +
      labs(x = paste0("mean_precip"
      )
      )
  }
  
  else{ print("NO DATA") }
  
  if(
    length(corresponding_table[,"rh98"])>0 # s'il y a des donn?es
  ){
    plot_rh98 <- ggplot(
      corresponding_table,
      aes(x=rh98)
    ) +
      geom_histogram(
        position="identity",
        bins = 30,
        colour = "white",
        fill = "limegreen",
        alpha = 0.6
      ) +
      labs(x = paste0("rh98"
      )
      )
  }
  else{ print("NO DATA") }
  
  plot <- grid.arrange(
    plot_cc, plot_rh98, plot_fire, plot_rain,
    nrow = 2, ncol=2,
    bottom = textGrob(ecoregion,gp=gpar(fontsize=20,font=1))
  )
  
  ggsave(
    file.path(path_to_Savanna_structure_GEDI_folder,"figures","hist_13Mai",
              paste0("hist_postsubsampling_",ecoregion,".png")),
    plot = plot,
    scale = 2, # au pif total du premier coup et ça marche :D
    bg='#ffffff'# pour ne pas avoir un fond gris moche
  )
  
}

