# Cleaning the environment
rm(list=ls())
# Getting the paths
source("paths.R")
getwd()

df_graphique = as.data.frame(matrix(nrow=4,ncol=2))
colnames(df_graphique) = c("pluie (mm yr-1)","biomasse (t.ha-1)")
df_graphique2 = df_graphique

df_graphique[1,] = c(400,2.3)
df_graphique[2,] = c(500,3)
df_graphique[3,] = c(600,4.2)
df_graphique[4,] = c(700,5.2)
df_graphique[5,] = c(750,6)
df_graphique[6,] = c(1000,8.75)

df_graphique2[1,] = c(400,2.3)
df_graphique2[2,] = c(500,3)
df_graphique2[3,] = c(600,4.3)
df_graphique2[4,] = c(700,6.5)
df_graphique2[5,] = c(800,9)
df_graphique2[6,] = c(900,12)


sigmo_pluie <- function(W,lambda,decalage){
  return(
    1/(1+exp(-lambda*(W-decalage)))
    )
}

sigmo_coeff_competition <- function(W,delta,decalage){
  return(
    1/(1+exp(-delta*(-W+decalage)))
    )
}

omega <- function(G,point_inflexion){return((G**2)/((G**2)+(point_inflexion**2)))}

display_mcmc_output_sigmoides = function(K_G_t_f_pluie_max_mcmc,lambda_mcmc,pt_inflexion_feu_mcmc,pt_inflexion_grass_mcmc){
  
  pentes = seq(0.00043,0.0022,length=20)
  lambdas = 4*pentes
  # print("pentes")
  # print(pentes)
  # print("lambdas")
  # print(lambdas)
  
  pente1 = 0.00688
  pente2 = 0.0022

  lambda1 = 0.00172
  lambda2 = 0.0088
  
  max_W = 3000
  W_values = seq(-max_W,max_W,by=10)

  # Premier graphique
    
  print("valeurs lambda_mcmc_possibles")

  palette <- rainbow(length(lambdas))
  par(mfrow = c(1, 2))
  plot.new()
  # legend("center", legend = paste("lambda=",round(lambdas,4),"pente=",round(lambdas/4,4)), col = palette, lty = 1,xpd = TRUE)
  legend("center", legend = paste("lambda=",round(lambdas,4)), col = palette, lty = 1,xpd = TRUE)
  
  plot(
       x=c(0),
       y=c(0),
       xlim=c(-1500,1500),
       xaxt = "n",
       ylim = c(0,1),
       xlab = "W", ylab = "grass (à constante multiplicative près)",
       main = "range de lambdas possibles"
       )
  
  for(i in 1:length(lambdas)){
     grass = sigmo_pluie(W_values,lambdas[i],decalage = 0)
     lines(W_values, grass, col = palette[i])
     abline(a=0.5,b=lambdas[i]/4,lty=2,col = palette[i])
     }
    
    abline(b=pente1,lwd=3)
    abline(b=pente2,lwd=3)
  
    abline(v=0,lty=2)
    
    # Second graphique
  
    print("donnees post mcmc")

    W_values = seq(-1500,2000,by=10)
    K_G_pluie_max = K_G_t_f_pluie_max_mcmc
      
    palette <- rainbow(length(lambdas))
    par(mfrow = c(1, 2))
    plot.new()

    legend(
      "center", 
      legend = rbind(
                     paste("lambda_mcmc=",round(lambda_mcmc,4),"yr.mm-1"),
                     paste("K_G_mcmc=",round(K_G_t_f_pluie_max_mcmc,4),"t.ha-1")
                     ),
      lty = 1,
      xpd = TRUE
      )
    
    plot(
         x=c(0),
         y=c(0),
         xlim=c(0,3500),
         ylim = c(0,K_G_pluie_max),
         xlab = "W", ylab = "mcmc_simulated_grass",
         main = paste0("pt_inflexion=",pt_inflexion_grass_mcmc,"mm.yr-1")
         )
  
    grass_simulated_by_mcmc = K_G_pluie_max*sigmo_pluie(W_values,lambda=lambda_mcmc,decalage = pt_inflexion_grass_mcmc)
    lines(W_values+pt_inflexion_grass_mcmc, grass_simulated_by_mcmc, col = "black", lwd = 3)
    
    abline(v=0,lty=2)
    abline(h=K_G_pluie_max,lty=2)
    lines(df_graphique[,"pluie (mm yr-1)"],df_graphique[,"biomasse (t.ha-1)"],pch=1,
          col="deeppink",lwd=3)
    lines(df_graphique2[,"pluie (mm yr-1)"],df_graphique2[,"biomasse (t.ha-1)"],pch=1,
          col="deeppink",lwd=3)
    
    # print(paste("lambda1=",lambda1,"pente1=",pente1))
    # print(paste("lambda2=",lambda2,"pente2=",pente2))
  
    # Troisième graphique
  
    print("donnees post mcmc")
    
    W_values = seq(-1500,2000,by=10)
    K_G_pluie_max = K_G_t_f_pluie_max_mcmc
      
    palette <- rainbow(length(lambdas))
    par(mfrow = c(1, 2))
    plot.new()
    legend("center", legend = paste("lambda=",round(lambdas,4)), col = palette, lty = 1,xpd = TRUE)

    
    plot(
         x=c(0),
         y=c(0),
         xlim=c(0,3500),
         ylim = c(0,K_G_pluie_max),
         xlab = "W", ylab = "",
         main = paste0("pt_inflexion=",pt_inflexion_grass_mcmc,"mm.yr-1")
         )
  
  for(i in 1:length(lambdas)){
       grass = K_G_pluie_max*sigmo_pluie(W_values,lambda=lambdas[i],decalage = pt_inflexion_grass_mcmc)
       lines(W_values+pt_inflexion_grass_mcmc, grass, col = palette[i])
       # abline(a=0.5,b=lambdas[i]/4,lty=2,col = palette[i])
  }
    
    grass_simulated_by_mcmc = K_G_pluie_max*sigmo_pluie(W_values,lambda=lambda_mcmc,decalage = pt_inflexion_grass_mcmc)
    lines(W_values+pt_inflexion_grass_mcmc, grass_simulated_by_mcmc, col = "black", lwd = 3)
    
    abline(v=0,lty=2)
    abline(h=K_G_pluie_max,lty=2)
    lines(df_graphique[,"pluie (mm yr-1)"],df_graphique[,"biomasse (t.ha-1)"],pch=1,
          col="deeppink",lwd=3)
    lines(df_graphique2[,"pluie (mm yr-1)"],df_graphique2[,"biomasse (t.ha-1)"],pch=1,
          col="deeppink",lwd=3)
    
    # print(paste("lambda1=",lambda1,"pente1=",pente1))
    # print(paste("lambda2=",lambda2,"pente2=",pente2))
    
    # Quatrième graphique
    
    par(mfrow = c(1, 2))
    plot.new()

    G = seq(0.00001,16,length=10**3)
    p_feu = omega(G,5)
    
    legend(
          "center", 
          legend = rbind(
                         paste("inflexion_feu_mcmc=",round(pt_inflexion_feu_mcmc,4),"t.ha-1")
                         ),
          lty = 1,
          xpd = TRUE
      )
    
    plot(G,
         p_feu,
         main=paste("omega(G)"),
         xlim=c(0,16),
         xlab = "t.ha-1",
         ylab = "p_feu"
         )
    
} # end of display_mcmc_output_sigmoides function definition

source(file.path(path_to_Savanna_structure_GEDI_folder,"R","complete_model_jags2.R"))
display_mcmc_output_sigmoides(K_G_t_f_pluie_max_mcmc,lambda_mcmc,pt_inflexion_feu_mcmc,pt_inflexion_grass_mcmc)
