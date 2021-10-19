// Combine nested regression effects
// 
// Combines nested regression effects using a design matrix
// includes effect pooling used a second design matrix in which
// the first row indicates no scaling (i.e  independent effects).
// 
// @param intercept The regression intercept
//
// @param beta A Vector of effects. In general these should be specified
// on the unit scale as they may be rescaled (and hence pooled) using the
// beta_sd vector.
//
// @param design The design matrix for the observations (rows) and effects
// columns.
//
// @param beta_sd A vector of standard deviations to use for scaling (pooling)
// effect sizes.
//
// @param sd_design The design matrix relating effect sizes (rows) with standard
// deviations (columns). The first column is used to indicate no scaling. In
// general each effect should only be scaled with a single standard deviation.
// 
// @return A vector of linear predictions without error.
vector combine_effects(real intercept, vector beta, matrix design,
                       vector beta_sd, matrix sd_design) {
  int nobs = cols(beta);
  int effs = rows(beta);
  int sds = num_elements(beta_sd);
  vector[nobs] y;
  vector[neffs] scaled_beta;
  vector[sds + 1] ext_beta_sd = col_append(1, beta_sd);

  scaled_beta = beta .* (sd_design * ext_beta_sd);
  print(scaled_beta);
  y = intercept + design * scaled_beta;
  return(y);
}
