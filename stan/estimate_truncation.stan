functions {
#include functions/regression.stan
#include functions/pmfs.stan
#include functions/observation_model.stan
}

data {
  int t;
  int nobs;
  int obs[t, nobs];
  int tdist[nobs];
  int tmax;
  int neffs;
  int nnest;
  matrix[nobs, neffs] emat;
  matrix[neffs, nnest + 1] nmat;
}

parameters {
  real<lower=0> uobs_logsd;
  real log_uobs_resids[tmax];
  real logmean_init;
  real<lower=0> logsd_init;
  vector[neffs] logmean_eff;
  vector[neffs] logsd_eff;
  vector<lower=0>[nnest] logmean_sd;
  vector<lower=0>[nnest] logsd_sd;
  real<lower=0> phi;
}

transformed parameters{
  vector[nobs] logmean;
  vector<lower=0>[nobs] logsd;
  matrix[tmax, nobs] cmfs;
  matrix[tmax, nobs] trunc_obs;
  real sqrt_phi;
  vector[tmax] imputed_obs;
  // calculate log mean and sd parameters for each dataset from design matrices
  logmean = combine_effects(logmean_init, logmean_eff, emat, logmean_sd, nmat);
  logsd = combine_effects(log(logsd_init), logsd_eff, emat, logsd_sd, nmat);
  logsd = exp(logsd);
  // calculate cmfs for each dataset
  for (i in 1:nobs) {
    cmfs[, i] = truncation_cmf(logmean[i], logsd[i], tmax);
  }
  {
  vector[t] last_obs;
  // reconstruct expected reported data as a latent parameter
  last_obs = to_vector(obs[, nobs]);
  for (i in 1:tmax) {
    int j = t - tmax + i;
    last_obs[j] = exp(log(last_obs[j - 1]) + log_uobs_resids[i] * uobs_logsd);
    imputed_obs[i] = last_obs[j];
  }
  // apply truncation to expected reported to map back to previous data sets
  for (i in 1:nobs) {
   int end_t = t - tdist[i];
   int start_t = end_t - tmax + 1;
   trunc_obs[, i] = truncate(last_obs[start_t:end_t], to_vector(cmfs[, i]), 0,
                             0);
   }
  }
  // Transform phi to overdispersion scale
  sqrt_phi = 1 / sqrt(phi);
}

model {
  // priors for unobserved expected reported cases
  uobs_logsd ~ normal(0, 5) T[0,];
  log_uobs_resids ~ std_normal();
  // priors for the intercept of the log normal truncation distribution
  logmean_init ~ normal(0, 1);
  logsd_init ~ normal(0, 1) T[0,];
  // Priors for effects on truncation distribution
  for (i in 1:nnest) {
    logmean_sd[i] ~ normal(0, 0.1) T[0,];
    logsd_sd[i] ~ normal(0, 0.1) T[0,];
  }
  logmean_eff ~ std_normal();
  logsd_eff ~ std_normal();
  // Reporting overdispersion (1/sqrt)
  phi ~ normal(0, 1) T[0,];
  // log density of truncated latest data vs that observed
  for (i in 1:nobs) {
    int start_t = t - tdist[i] - tmax;
    for (j in 1:tmax) {
      obs[start_t + j, i] ~ neg_binomial_2(trunc_obs[j, i] + 1e-3, sqrt_phi);
    }
  }
}

generated quantities {
  int recon_obs[tmax, nobs];
  int sim_trunc_obs[tmax, nobs];
  int sim_imputed_obs[tmax];
  // reconstruct all truncated datasets using generative model
  // also apply truncation to observations to reconstruct unobserved obs
  for (i in 1:nobs) {
    int end_t = t - tdist[i];
    int start_t = end_t - tmax + 1;
    vector[tmax] mean_recon_obs;
    mean_recon_obs = truncate(to_vector(obs[start_t:end_t, i]), 
                              to_vector(cmfs[, i]), 1,  1);
    recon_obs[, i] = neg_binomial_2_rng(mean_recon_obs + 1e-3, sqrt_phi);
    sim_trunc_obs[, i] = neg_binomial_2_rng(trunc_obs[, i] + 1e-3, sqrt_phi);
  }
  // Combine observed and imputed observations with
  // reporting noise for the latest dataset
  {
    int last_obs[tmax] = obs[(t - tmax + 1):t, nobs];
    vector[tmax] imp_uobs = imputed_obs .* (1 - cmfs[,  nobs]);
    int sim_imp_uobs[tmax] = neg_binomial_2_rng(imp_uobs + 1e-3, sqrt_phi);
    for (i in 1:tmax) {
      sim_imputed_obs[i] = last_obs[i] + sim_imp_uobs[i];
    }
  }
}
