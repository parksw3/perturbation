data {
  int N;
  int week[N];
  int cases[N];
  real birth[N];
  real pop;
  real alpha;
  real rhomean;
}

parameters {
  real<lower=0, upper=1> S0;
  real<lower=0, upper=1> I0;
  real<lower=0, upper=1> rho;
  real<lower=0> beta[52];
  real<lower=0> phi;
}

transformed parameters {
  vector[N] S;
  vector[N] I;
  
  S[1] = S0 * pop;
  I[1] = I0 * pop;
  
  for (i in 2:N) {
    I[i] = beta[week[i]] * S[i-1] * (I[i-1])^alpha/pop;
    S[i] = S[i-1] + birth[i] - I[i];
  }
}

model {
  beta ~ normal(20, 5);
  S0 ~ beta(2, 98);
  I0 ~ uniform(0, 1);
  rho ~ normal(rhomean, 0.02);
  phi ~ normal(0, 10);
  
  cases ~ neg_binomial_2(I*rho, phi);
}
