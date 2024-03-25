// Regression linéaire standard

data { 
      int<lower=0> n;
      vector[n] canopy_cover;
      vector[n] mean_precip;
}
parameters { 
            real intercept;
            real beta;
            real<lower=0> sigma;
}

model {
       canopy_cover ~ normal(intercept + beta * mean_precip, sigma);
}
