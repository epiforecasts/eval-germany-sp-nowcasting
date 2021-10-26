functions {
#include functions/regression.stan
#include functions/pmfs.stan
}

data {
  // currently a snapshot for every day is a requirement to get correct
  // posterior predictions of final reports.
  int t; // time range over which data is available 
  int s; // number of snapshots there are
  int g; // number of data groups
  int st[s]; // when in this time snapshots are from
  int ts[t, g]; // snapshot related  to time and group
  int sl[s]; // how many days of reported data does each snapshot have
  int sg[s]; // how snapshots are related
  int dmax; // maximum possible report date
  int obs[s, dmax]; // obs for each primary date (row) and report date (column)
  int latest_obs[t, g]; // latest obs for each snapshot group
  int npmfs; // how many unique pmfs there are
  int spmfs[s]; // how each snapshot links to a pmf
  int neffs; // number of effects to apply
  matrix[npmfs, neffs + 1] design; // design matrix for pmfs
  int neff_sds; // number of standard deviations to use for pooling
  matrix[neffs, neff_sds + 1] design_sd; // Pooling pmf design matrix
  int dist; // distribution used for pmfs (0 = lognormal, 1 = gamma)
  // design matrix for dates of report
  //lLinkage between dates of report and report_design matrix
  int debug; // should debug information be shown
  int likelihood; // should the likelihood be included
  int pp; // should posterior predictions be produced
  int cast; // should a nowcast be produced
}

transformed data{
  real logdmax = log(dmax); // scaled maxmimum delay to log for crude bounds
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
  vector<lower=-10, upper=logdmax>[npmfs] logmean;
  vector<lower=1e-3, upper=dmax>[npmfs] logsd;
  matrix[dmax, npmfs] pmfs;
  real phi;
  vector[dmax] imp_obs[g];
  // calculate log mean and sd parameters for each dataset from design matrices
  logmean = combine_effects(logmean_int, logmean_eff, design, logmean_sd,
                            design_sd);
  logsd = combine_effects(log(logsd_int), logsd_eff, design, logsd_sd,
                          design_sd);
  logsd = exp(logsd);
  // calculate pmfs
  for (i in 1:npmfs) {
    pmfs[, i] = calculate_pmf(logmean[i], logsd[i], dmax, dist);
  }
  // estimate unobserved final reported cases for each group
  // this could be any forecasting model but here its a 
  // first order random walk for each group on the log scale.
  for (k in 1:g) {
    real llast_obs;
    for (i in 1:dmax) {
      if (i == 1) {
        llast_obs = obs[t - dmax, k];
      }else{
        llast_obs = imp_obs[k][i - 1];
      }
      llast_obs = log(llast_obs);
      imp_obs[k][i] = exp(llast_obs + log_uobs_resids[k][i] * uobs_logsd[k]);
    }
  }
  // transform phi to overdispersion scale
  phi = 1 / sqrt(sqrt_phi);

  // debug issues in truncated data if/when they appear
  if (debug) {
#include /chunks/debug.stan
  }
}
  
model {
  // priors for unobserved expected reported cases
  for (i in 1:g) {
    uobs_logsd[i] ~ normal(0, 5) T[0,];
    log_uobs_resids[i] ~ std_normal();
  }
  // priors for the intercept of the log normal truncation distribution
  logmean_int ~ normal(0, 1);
  logsd_int ~ normal(0, 1);

  // priors and scaling for date of reference effects
  if (neffs) {
    logmean_eff ~ std_normal();
    logsd_eff ~ std_normal();
    if (neff_sds) {
      for (i in 1:neff_sds) {
        logmean_sd[i] ~ normal(0, 0.1) T[0,];
        logsd_sd[i] ~ normal(0, 0.1) T[0,];
      }
    }
  }
  // reporting overdispersion (1/sqrt)
  sqrt_phi ~ normal(0, 1) T[0,];
  // log density: observed vs model
  if (likelihood) {
    real target_obs;
    for (i in 1:s) {
      vector[sl[i]] exp_obs;
      if (st[i] <= (t - dmax)) {
        target_obs = latest_obs[st[i], sg[i]];
      }else{
        target_obs = imp_obs[sg[i]][st[i] - (t - dmax)];
      }
      exp_obs = target_obs * pmfs[1:sl[i], spmfs[i]] + 1e-3;
      obs[i, 1:sl[i]] ~ neg_binomial_2(exp_obs, phi);
    }
  }
}

generated quantities {
  int pp_obs[pp ? s : 0, pp ? dmax : 0];
  int pp_inf_obs[dmax, g];
  if (cast) {
    int pp_obs_tmp[s, dmax];
    vector[dmax] exp_obs;
    real target_obs;
    // Posterior predictions for observations
    for (i in 1:s) {
      if (st[i] <= (t - dmax)) {
        target_obs = latest_obs[st[i], sg[i]];
      }else{
        target_obs = imp_obs[sg[i]][st[i] - (t - dmax)];
      }
      exp_obs = target_obs * pmfs[1:dmax, spmfs[i]] + 1e-3;
      pp_obs_tmp[i, 1:dmax] = neg_binomial_2_rng(exp_obs, phi);
    }

    // Posterior prediction for final reported data (i.e at t = inf)
    for (k in 1:g) {
      int start_t = t - dmax;
      for (i in 1:dmax) {
        int snap = ts[start_t + i, k];
        pp_inf_obs[i, k] = sum(obs[snap, 1:sl[snap]]);
        pp_inf_obs[i, k] += sum(pp_obs_tmp[snap, (sl[snap]+1):dmax]);
      }
    }
    if (pp) {
    pp_obs = pp_obs_tmp;
    }
  }
}
