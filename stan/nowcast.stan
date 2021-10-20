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
  int neff_sds;
  matrix[nobs, neffs ? neffs : 1] design;
  matrix[neffs, neff_sds + 1] design_sd;
  int debug;
  int likelihood;
}

transformed data{
  real logtmax = log(tmax);
}

parameters {
  real<lower=0> uobs_logsd;
  real log_uobs_resids[tmax];
  real<lower=-10, upper=logtmax> logmean_init;
  real<lower=1e-3, upper = tmax> logsd_init;
  vector[neffs] logmean_eff;
  vector[neffs] logsd_eff;
  vector<lower=0>[neff_sds] logmean_sd;
  vector<lower=0>[neff_sds] logsd_sd;
  real<lower=0, upper=1e4> phi;
}

transformed parameters{
  vector<lower=-10, upper=logtmax>[nobs] logmean;
  vector<lower=1e-3, upper=tmax>[nobs] logsd;
  matrix[tmax, nobs] cmfs;
  matrix<lower=0>[tmax, nobs] trunc_obs;
  real sqrt_phi;
  vector[tmax] imputed_obs;
  // calculate log mean and sd parameters for each dataset from design matrices
  logmean = combine_effects(logmean_init, logmean_eff, design, logmean_sd,
                            design_sd);
  logsd = combine_effects(log(logsd_init), logsd_eff, design, logsd_sd,
                          design_sd);
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

  // Debug issues in truncated data if/when they appear
  if (debug) {
    for (i in 1:nobs) {
      int j = 0;
      for (k in 1:tmax) {
        j += is_nan(trunc_obs[k, i]) ? 1 : 0;
        j += sqrt_phi <= 1e-3 ? 1 : 0;
      }
      if (j) {
        print("Issue with Dataset");
        print(i);
        print("Posterior prediction (no observation error)");
        print(trunc_obs[, i]);
        print("Truncation  distribution estimate");
        print(cmfs[, i]);
        print("Logmean and Logsd intercept");
        print(logmean_init);
        print(logsd_init);
        print("Logmean and Logsd for each dataset");
        print(logmean[i]);
        print(logsd[i]);
        print("Overdispersion");
        print(sqrt_phi);
      }
    }
  }
}
  

model {
  // priors for unobserved expected reported cases
  uobs_logsd ~ normal(0, 5) T[0,];
  log_uobs_resids ~ std_normal();
  // priors for the intercept of the log normal truncation distribution
  logmean_init ~ normal(0, 1);
  logsd_init ~ normal(0, 1);
  // Priors for effects on truncation distribution
  for (i in 1:neff_sds) {
    logmean_sd[i] ~ normal(0, 0.1) T[0,];
    logsd_sd[i] ~ normal(0, 0.1) T[0,];
  }
  if (neffs) {
    logmean_eff ~ std_normal();
    logsd_eff ~ std_normal();
  }
  // Reporting overdispersion (1/sqrt)
  phi ~ normal(0, 1) T[0,];
  // log density of truncated latest data vs that observed
  if (likelihood) {
    for (i in 1:nobs) {
      int start_t = t - tdist[i] - tmax;
      for (j in 1:tmax) {
        obs[start_t + j, i] ~ neg_binomial_2(trunc_obs[j, i] + 1e-3, sqrt_phi);
      }
    }
  }
}

generated quantities {
  int recon_obs[tmax, nobs];
  int sim_trunc_obs[tmax, nobs];
  int sim_imputed_obs[tmax];
  // reconstruct all truncated datasets using generative model
  // also apply truncation to observations to approximate
  // reconstructing unobserved obs
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
    int sim_imp_uobs[tmax];
    for (i in 1:tmax) {
      imp_uobs[i] = max({1e-3, imp_uobs[i]});
    }
    sim_imp_uobs = neg_binomial_2_rng(imp_uobs + 1e-3, sqrt_phi);
    for (i in 1:tmax) {
      sim_imputed_obs[i] = last_obs[i] + sim_imp_uobs[i];
    }
  }
}
