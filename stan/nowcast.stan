functions {
#include functions/regression.stan
#include functions/pmfs.stan
}

data {
  int t; // time range over which data is available 
  int s; // number of snapshots there are
  int st[s]; // When in this time snapshots are from
  int ts[t, g]; // Snapshot related  to time and group
  // Currently a snapshot for every day is a requirement to get correct
  // posterior predictions of final reports.
  int sl[s]; // How many days of reported data does each snapshot have
  int g; // Number of data groups
  int sg[s]; // How snapshots are related
  int dmax; // Maximum possible report date
  int obs[s, dmax]; // obs for each primary date (row) and report date (column)
  int latest_obs[t, g]; // latest obs for each snapshot group
  int ncmfs; // How many unique cmfs there are
  int scmfs[s]; // How each snapshot links to a cmf
  matrix[ncmfs, neffs ? neffs : 1] design; // Design matrix for CMFs
  matrix[neffs, neff_sds + 1] design_sd; // Pooling CMF design matrix
  int dist; // Distribution used for CMFs (0 = lognormal, 1 = gamma)
  // Design matrix for dates of report
  // Linkage between dates of report and report_design matrix
  int debug; // Should debug information be shown
  int likelihood; // Should the likelihood be included
  int pp; // Should posterior predictions be produced
}

transformed data{
  real logdmax = log(dmax); // Scaled maxmimum delay to log for crude bounds
}

parameters {
  real<lower=0> uobs_logsd[g]; // standard deviation of rw for primary obs 
  vector[dmax] log_uobs_resids[g]; // unscaled rw for primary obs
  real<lower=-10, upper=logdmax> logmean_int; // logmean intercept
  real<lower=1e-3, upper = dmax> logsd_int; // logsd intercept
  vector[neffs] logmean_eff; // unscaled modifiers to log mean
  vector[neffs] logsd_eff; // unscaled modifiers to log sd
  vector<lower=0>[neff_sds] logmean_sd; // pooled modifiers to logmean
  vector<lower=0>[neff_sds] logsd_sd; // ppoled modifiers to logsd
  real<lower=0, upper=1e4> sqrt_phi; // Overall dispersion by group
}

transformed parameters{
  vector<lower=-10, upper=logdmax>[ncmfs] logmean;
  vector<lower=1e-3, upper=dmax>[ncmfs] logsd;
  matrix[dmax, ncmfs] cdfs;
  real phi;
  vector[dmax] imp_obs[g];
  // calculate log mean and sd parameters for each dataset from design matrices
  logmean = combine_effects(logmean_init, logmean_eff, design, logmean_sd,
                            design_sd);
  logsd = combine_effects(log(logsd_init), logsd_eff, design, logsd_sd,
                          design_sd);
  logsd = exp(logsd);
  // calculate cmfs
  for (i in 1:ncmfs) {
    cmfs[, i] = calculate_cmf(logmean[i], logsd[i], dmax, dist);
  }
  for (k in 1:g) {
    real llast_obs;
    for (i in 1:tdmax) {
      int j = t - tmax + i;
      if (i == 1) {
        llast_obs = obs[j -1, k];
      }else{
        llast_obs = imp_obs[k][i - 1];
      }
      llast_obs = log(last_obs)
      imp_obs[k][i] = exp(last_obs + log_uobs_resids[k][i] * uobs_logsd[k]);
    }
  }
  // Transform phi to overdispersion scale
  phi = 1 / sqrt(sqrt_phi);

  // Debug issues in truncated data if/when they appear
  if (debug) {
#include /chunks/debug.stan
  }
}
  
model {
  // priors for unobserved expected reported cases
  for (i in 1:g) {
    uobs_logsd[g] ~ normal(0, 5) T[0,];
    log_uobs_resids[g] ~ std_normal();
  }
  // priors for the intercept of the log normal truncation distribution
  logmean_int ~ normal(0, 1);
  logsd_int ~ normal(0, 1);
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
  sqrt_phi ~ normal(0, 1) T[0,];
  
  // log density vs that observed
  if (likelihood) {
    for (k in 1:g) {
      for (i in 1:s) {
        real target_obs;
        if (st[i] < (t - dmax)) {
          target_obs = latest_obs[st[i], k];
        }else{
          target_obs = imp_obs[k][st[i]];
        }
        vector[sl[i]] exp_obs = target_obs .* cmfs[1:sl[i], scmfs[i]] + 1e-3;
        obs[1:sl[i], i] ~ neg_binomial_2(exp_obs, phi);
      }
    }
  }
}

generated quantities {
  int pp_obs[s, dmax];
  int pp_inf_obs[dmax, g];
  if (pp) {
    // Posterior predictions for observations
    for (k in 1:g) {
      for (i in 1:s) {
        real target_obs;
        if (st[i] < (t - dmax)) {
          target_obs = latest_obs[st[i], k];
        }else{
          target_obs = imp_obs[k][st[i]];
        }
        vector[dmax] exp_obs = target_obs .* cmfs[, scmfs[i]] + 1e-3;
        pp_obs[, i] ~ neg_binomial_2(exp_obs, phi);
      }
    }
    // Posterior prediction for final reported data (i.e at t = inf)
    for (k in 1:g) {
      int start_t = t - dmax;
      for (i in 1:dmax) {
        int snap = ts[start_t + i, k];
        pp_inf_obs[i] = sum(obs[1:sl[snap], snap]);
        pp_inf_obs[i] += sum(pp_obs[(sl[snap]+1):dmax, snap]);
      }
    }
  }
}
