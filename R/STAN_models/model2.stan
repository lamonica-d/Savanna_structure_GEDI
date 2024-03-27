// Regression linéaire standard

data { 
      int<lower=0> n;
      int<lower=0> K;
      matrix[n,K] x;
      vector[n] canopy_cover;
}
parameters { 
            real intercept;
            vector[K] beta;
            real<lower=0> sigma;
}

model {
       canopy_cover ~ normal(intercept + x*beta, sigma);
}
