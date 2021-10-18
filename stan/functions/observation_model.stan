// Calculate a truncation CMF
vector truncation_cmf(real trunc_mean, real trunc_sd, int trunc_max) {
    int  trunc_indexes[trunc_max];
    vector[trunc_max] cmf;
    for (i in 1:(trunc_max)) {
      trunc_indexes[i] = i - 1;
    }
    cmf = discretised_lognormal_pmf(trunc_indexes, trunc_mean, trunc_sd, trunc_max);
    cmf[1] = cmf[1] + 1e-8;
    cmf = cumulative_sum(cmf);
    cmf = reverse_mf(cmf, trunc_max);
    return(cmf);
}
// Truncate with a supplied cmf
vector truncate(vector reports, vector cmf, int reconstruct, int pad) {
  int t = num_elements(reports);
  int cmf_max = num_elements(cmf);
  vector[t] trunc_reports = reports;
  if (pad) {
    trunc_reports = trunc_reports + rep_vector(1e-1, t);
  }
  // Find actual min length of cmf and reports
  // Calculate cmf of truncation delay
  {
    int trunc_max = cmf_max > t ? t : cmf_max;
    vector[trunc_max] cmf_trunc = cmf[1:trunc_max];
    int first_t = t - trunc_max + 1;
    // Apply cdf of truncation delay to truncation max last entries in reports
    if (reconstruct) {
      trunc_reports[first_t:t] = trunc_reports[first_t:t] ./ cmf_trunc;
    }else{
      trunc_reports[first_t:t] = trunc_reports[first_t:t] .* cmf_trunc;
    }
  }
  return(trunc_reports);
}
// Truncation distribution priors
void truncation_lp(real[] truncation_mean, real[] truncation_sd,
                   real[] trunc_mean_mean, real[] trunc_mean_sd,
                   real[] trunc_sd_mean, real[] trunc_sd_sd) {
  int truncation = num_elements(truncation_mean);
  if (truncation) {
    truncation_mean ~ normal(trunc_mean_mean, trunc_mean_sd);
    truncation_sd ~ normal(trunc_sd_mean, trunc_sd_sd);
  }
}
