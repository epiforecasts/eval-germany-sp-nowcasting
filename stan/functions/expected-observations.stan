vector prob_to_hazard(vector p) {
  int l = num_elements(p);
  vector[l] h;
  vector[l] cum_p;
  cum_p = cumulative_sum(cum_p);
  h = p ./ (1 - cum_p);
  return(h);
}

vector hazard_to_prob(vector h) {
  int l = num_elements(h);
  vector[l] p;
  real p_sum = 0;
  for (i in 1:l) { 
    p[i] = (1 - p_sum) * h[1];
    p_sum += p[i];
  }
  p[l] = 1 - sum(p[1:(l-1)]);
  return(p);
}

vector expected_obs(real tar_obs, vector date_p, vector rd_lhazard) {
  int t = num_elements(date_p);
  vector[t] exp_obs;
  real total_rd_lhazard = sum(rd_lhazard);
  if (total_rd_lhazard == 0) {
    exp_obs = tar_obs * date_p + 1e-3;
  }else{
    vector[t] date_h;
    vector[t] hazard;
    vector[t] p;
    date_h = prob_to_hazard(date_p);
    hazard = inv_logit(logit(date_h) + rd_lhazard);
    p = hazard_to_prob(hazard);
    exp_obs = tar_obs * p + 1e-3;
  }
  return(exp_obs);
}
