
# Cleaning the environment
rm(list=ls())
# Getting the paths
source("R/paths.R")

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
print(colnames(Guinean_table))


# vignette("brms_families")
# browseURL("https://rdrr.io/cran/brms/man/brmsfamily.html")

default_prior = get_prior(
  formula = rh98 ~ mean_precip + mean_temp  + fire_freq,
  data = Guinean_table,
  family = brmsfamily(family = "Gamma")
)

View(default_prior)

sub_Guinean_table <- Guinean_table[complete.cases(Guinean_table[,c("mean_precip",
                                                                   "mean_temp",
                                                                   "fire_freq")]
),]
sub_Guinean_table <- sub_Guinean_table[1:10**6,]

start <- Sys.time()
print(start)

mod <- brm(
  
  formula = rh98 ~ mean_precip + mean_temp  + fire_freq,
 data = sub_Guinean_table,
  
  family = brmsfamily(family = "Gamma"),
 prior = NULL,
 
  warmup = 1*10**3,
  iter = 2*10**3,
  thin = 10,
  
  file = "outputs/test_brms_Guinean_dom1.RDS",
  
  chains = 3,
  cores = 3,    
  silent = 0
)

print(Sys.time() - start)
#Time difference of 1.241757 hours pour 0

summary(mod)
plot(mod)
setwd(path_to_R_folder)

# Pour enregistrer manuellement la chi?ne sans utiliser l'argument "file" dans brm() :

# saveRDS(mod,file="test_brms_Guinean2.RDS")
# mod2 <- readRDS("test_brms_Guinean2.RDS")

# Pour enregistrer en pdf le visuel des chaines :

# pdf("chaines_Guinean_100000_lignes.pdf")
# plot(mod,ask=FALSE)
# dev.off()

posterior_summary(mod)

ok <- as.data.frame(mod)
View(ok)


