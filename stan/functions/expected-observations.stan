// Calculate expected observations
// 
// Calculate expected observations over time from a combination of final
// expected observations, the probability of reporting on a given day effects
// that occured the reference day, and the hazard of reporting on a given day
// given reporting day effects. 
// 
// @param tar_obs The final expected observations that will be reported for
// a given date.
// 
// @param date_p A vector of conditional probabilities a report occurs on a
// given day. Designed to take a input based on when the observation occurred
// 
// @param rd_lhazard A vector of logit hazards (i.e conditional probabilities) 
// for day of report effects
// 
// @return A vector of expected observations for a given date by date of report
// 
// @examples
// # compile function for use in R
// source(here::here("R", "utils.R"))
// expose_stan_fns(c("hazard.stan", "expected-observations.stan"),
//                 "stan/functions")
//
// tar_obs <- 1
// date_p <- (plnorm(1:30, 1.5, 2) - plnorm(0:29, 1.5, 2)) / plnorm(30, 1.5, 2)
// rd_lhazard <- rep(0, 30)
//
// Example with no reporting day effect
// eobs <- expected_obs(tar_obs, date_p, rep(0, 30))
// all.equal(eobs - 1e-4, date_p)
//
// Example with hazard effect only on last day of report
// eobs <- expected_obs(tar_obs, date_p, c(rep(0, 29), 0.1))
// all.equal(eobs - 1e-4, date_p)
//
// Example with a single day of additional reporting hazard and
// no differential probability due to the reference date.
// rd_lhazard <- rep(0, 30); rd_lhazard[7] = 2
// round(expected_obs(tar_obs, rep(1/30, 30), rd_lhazard), 3)
// # 0.033 0.033 0.033 0.033 0.033 0.033 0.195 0.026 0.026 0.026 0.026 0.026
// # 0.026 0.026 0.026 0.026 0.026 0.026 0.026 0.026 0.026 0.026 0.026 0.026
// # 0.026 0.026 0.026 0.026 0.026 0.026
//
// Example combining multiple hazards and no differential prob due to reference
// date
// rd_lhazard <- rep(0, 30)
// rd_lhazard[c(6, 12, 16)] = 2
// rd_lhazard[c(2, 20)] = -2
// round(expected_obs(tar_obs, rep(1/30, 30), rd_lhazard), 3)
// # 0.033 0.005 0.034 0.034 0.034 0.202 0.027 0.027 0.027 0.027 0.027 0.151
// # 0.021 0.021 0.021 0.106 0.014 0.014 0.014 0.002 0.016 0.016 0.016 0.016
// # 0.016 0.016 0.016 0.016 0.016 0.016

//
// Example combining both date of reference and date of report effects
// eobs <- expected_obs(tar_obs, date_p, rd_lhazard)
// sum(eobs - 1e-4) == 1
vector expected_obs(real tar_obs, vector date_p, vector rd_lhazard) {
  int t = num_elements(date_p);
  vector[t] exp_obs;
  real total_rd_lhazard = sum(rd_lhazard);
  if (total_rd_lhazard == 0) {
    exp_obs = tar_obs * date_p + 1e-4;
  }else{
    vector[t] date_h;
    vector[t] hazard;
    vector[t] p;
    date_h = prob_to_hazard(date_p);
    hazard = inv_logit(logit(date_h) + rd_lhazard);
    p = hazard_to_prob(hazard);
    exp_obs = tar_obs * p + 1e-4;
  }
  return(exp_obs);
}
