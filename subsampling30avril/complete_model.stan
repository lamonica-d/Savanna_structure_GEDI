// First attempt

functions {
  real sigmo_pluie(real lambda,real W,real pt_inflexion){
    if (lambda < 0 || W < 0 || pt_inflexion < 0){ reject("sigmo_pluie input out of bounds"); }
    real grass;
    grass = 1/( 1+exp(-lambda*(W-pt_inflexion)) );
    if (grass < 0 || grass > 1){ reject("sigmo_pluie output out of bounds"); }
    return grass;
  }
  
  real sigmo_competition(real delta,real W,real pt_inflexion){
    if (delta < 0 || W < 0 || pt_inflexion < 0){ reject("sigmo_competition input out of bounds"); }
    real competition_coeff;
    competition_coeff = 2/( 1+exp(delta*(W-pt_inflexion)) );
    if (competition_coeff < 0 || competition_coeff > 2){ reject("sigmo_competition output out of bounds"); }
    return competition_coeff;
  }
  
  real delta_intensity(real cc, real delta_min, real delta_max){
    if (cc < 0 || cc > 1 || delta_min < 0 || delta_max < 0){ reject("delta_intensity input out of bounds"); }
    return delta_max + (1-cc)*delta_min;
  }
  
  real omega(real G, real pt_inflexion){
    if (G < 0 || G > 30 || pt_inflexion < 0 ){ reject("omega input out of bounds"); }
    real p_feu;
    p_feu = pow(G,2)/(pow(G,2)+pow(pt_inflexion,2));
    if (p_feu < 0 || p_feu > 1){ reject("omega output out of bounds"); }
    return p_feu;
  }
}

data { 
      int<lower=0> N;
      vector[N] prec_data;
      vector[N] fire_data;
      vector[N] cc_data;
      real<lower=0> delta_min;
      real<lower=0> delta_max;
      }
parameters { 
      real<lower=0,upper=30> K_G_t_f_pluie_max; // autour de 16 t.ha-1, varie selon la zone
      real<lower=0,upper=4500> pt_inflexion_grass; // pt inflexion en fonction de la pluie <4500 mm.yr-1
      real<lower=0,upper=10> pt_inflexion_feu; // pt inflexion en fonction de la biomasse herbacée <10 t.ha-1
      vector<lower=delta_min,upper=delta_max>[N] lambda; // vectors are real no need to precise
      
      // pour les ensembles de définition, j'ai taillé large sauf pour lambda, à voir si c'est pertinent
}
transformed parameters {
   vector<lower=0,upper=30>[N] grassB; // 
   vector<lower=0,upper=1>[N] p_feu; // 
   real<lower=delta_min,upper=delta_max> delta;
   
   for (i in 1:N){
     
     delta = delta_intensity(cc_data[i],delta_min,delta_max);
     
     grassB[i] = K_G_t_f_pluie_max*sigmo_pluie(lambda[i],prec_data[i],pt_inflexion_grass)*
     sigmo_competition(delta,prec_data[i],pt_inflexion_grass);
   }
   
    for (i in 1:N){ p_feu[i] = omega(grassB[i],pt_inflexion_feu); }

}

model {
  real feu_simule;
  for (i in 1:N){ 
      feu_simule ~ normal(p_feu,0.1) ; // je pense qu'il faut être assez restrictif
      // sur l'écart-type si on veut vérifier que p_feu est proche de fire_data
      if(feu_simule < 0){fire_data[i] = 0.0;}
      else if(feu_simule > 1){fire_data[i] = 1.0;}
      else{fire_data[i] = feu_simule ;}
  }
}



