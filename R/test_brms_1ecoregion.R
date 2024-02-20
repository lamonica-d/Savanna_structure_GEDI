
# Cleaning the environment
rm(list=ls())
# Getting the paths
source("R/paths.R")# ,encoding="latin1")

# Libraries
library(fst)
library(ggplot2)
library(brms)


#setwd(path_to_GEDI_raw_data)
#names = c("Guinean_forest-savanna","West_Sudanian","Sahelian_Acacia")
#corresponding_tables = list()

# i = 0
# 
# for (name in names){
#   
#   i <- i +1
#   
#  print(name)
  corresponding_table <- fst::read.fst(paste0(path_to_GEDI_raw_data, 
                                              "/Guinean_forest-savanna.fst", sep=""))
  print(paste("Nb lignes :",round(nrow(corresponding_table),-3)))
  
  TRUE_FALSE_is_fire_freq_NA <- is.na(corresponding_table[,"fire_freq"])
  corresponding_table["fire_freq_NA"] <- TRUE_FALSE_is_fire_freq_NA
  
  # To replace the NA by zeros :
  corresponding_table[which(TRUE_FALSE_is_fire_freq_NA),"fire_freq"] <- 0
  
  # print("Mean_precip :")
  # print(summary(corresponding_table[,"mean_precip"]))
  
#   corresponding_tables[[i]] <- corresponding_table
#   
# }

Guinean_table <- corresponding_table #[[1]]
# Sudanian_table <- corresponding_tables[[2]]
# Sahelian_table <- corresponding_tables[[3]]

rm(corresponding_table) #,i,name)
rm(TRUE_FALSE_is_fire_freq_NA)

print(round(nrow(Guinean_table),-3))

summary(Guinean_table[,"rh98"])
# On va tester sur le rh98 avec une loi Gamma pour commencer
# pour ne pas avoir le probl?me du beta inflated avec canopy_cover dans [0,1[

print(colnames(Guinean_table))


# get_prior donne la liste des priors utilis?s par d?faut (qu'on peut changer individuellement)
# avec la commande brms pour une formule donn?e

# Pour avoir des d?tails sur une loi et se(s) lien(s) pour un glm :

# vignette("brms_families")
# browseURL("https://rdrr.io/cran/brms/man/brmsfamily.html")

# Pour plus d'infos j'ai mis toute la doc dans Forest_savanna_project -> _Documents -> brms

default_prior = get_prior(
  formula = rh98 ~ mean_precip + mean_temp  + fire_freq,
  # +  (1 | machin) pour effet al?atoire machin
  
  data = Guinean_table,
  
  family = brmsfamily(family = "Gamma")
  # ? voir comment sp?cifier le lien et lequel est utilis?
)

View(default_prior)

# Essai sur un sous-?chantillon

# On enl?ve les NA pour ?tre s?r de la taille du sous-?chantillon
sub_Guinean_table <- Guinean_table[complete.cases(Guinean_table[,c("mean_precip",
                                                                   "mean_temp",
                                                                   "fire_freq")]
),]
sub_Guinean_table <- sub_Guinean_table[1:10**5,]



start <- Sys.time()
print(start)

mod <- brm(
  
  formula = rh98 ~ mean_precip + mean_temp  + fire_freq,
  # +  (1 | machin) pour effet al?atoire machin
  
  data = sub_Guinean_table,
  
  family = brmsfamily(family = "Gamma"),
  # ? voir comment sp?cifier le lien et lequel est utilis?
  
  prior = NULL,
  # prior = NULL pour utiliser les priors par d?faut,
  
  warmup = 1*10**3,
  iter = 5*10**3,
  thin = 10,
  
  file = "outputs/test_brms_Guinean_dom0.RDS",
  
  chains = 3,
  cores = 3,          # Number of cores to use when executing the chains in parallel.
  
  # control = list(adapt_delta = 0.95), 
  # A named list of parameters to control the sampler's behavior.
 
  silent = 0
)

print(Sys.time() - start)
#Time difference of 1.241757 hours pour 0

summary(mod)
plot(mod)
setwd(path_to_R_folder)

# Pour enregistrer manuellement la cha?ne sans utiliser l'argument "file" dans brm() :

# saveRDS(mod,file="test_brms_Guinean2.RDS")
# mod2 <- readRDS("test_brms_Guinean2.RDS")

# Pour enregistrer en pdf le visuel des cha?nes :

# pdf("chaines_Guinean_100000_lignes.pdf")
# plot(mod,ask=FALSE)
# dev.off()

posterior_summary(mod)

ok <- as.data.frame(mod)
View(ok)


