// reverse a vector
vector reverse_vect(vector vect, int max_v) {
  vector[max_v] rev_vect;
  for (d in 1:max_v) {
    rev_vect[d] = vect[max_v - d + 1];
  }
  return rev_vect;
}
// Calculate a truncation using a parametric distribution
vector truncation_via_dist(real logmean, real logsd, int cdf_max) {
    vector[cdf_max] cdf;
    for (i in 1:cdf_max) {
      cdf[i] = lognormal_cdf(i, logmean, logsd);
    }
    cdf = cdf ./ max(cdf);
    cdf = reverse_vect(cdf, cdf_max);
    return(cdf);
}
// Truncate with a supplied vector
vector truncate(vector reports, vector trunc_vec, int reconstruct, int pad) {
  int t = num_elements(reports);
  int trunc_length = num_elements(trunc_vec);
  vector[t] trunc_reports = reports;
  if (pad) {
    trunc_reports = trunc_reports + rep_vector(1e-1, t);
  }
  // Find actual min length of vector and reports
  {
    int trunc_max = trunc_length > t ? t : trunc_length;
    vector[trunc_max] trunc_app = trunc_vec[1:trunc_max];
    int first_t = t - trunc_max + 1;
    // Apply cdf of truncation delay to truncation max last entries in reports
    if (reconstruct) {
      trunc_reports[first_t:t] = trunc_reports[first_t:t] ./ trunc_app;
    }else{
      trunc_reports[first_t:t] = trunc_reports[first_t:t] .* trunc_app;
    }
  }
  return(trunc_reports);
}
