enw_fit <- function(data, model, inits, ...) {
  if (is.null(model)) {
    model <- rstan::stan_model(here("stan", "nowcast.stan"))
  }
  fit <- rstan::sampling(model,
    data = data,
    init = inits,
    ...
  )
  return(fit)
}

epinowcast <- function(preprocessed_obs,
                       model = NULL, probs = c(0.2, 0.5, 0.9),
                       dist = "lognormal", date_design = NULL,
                       likelihood = TRUE, debug = FALSE, pp = TRUE,
                       ...) {
  stan_data <- enw_stan_data(preprocessed_obs,
    dist = dist, date_design = date_design,
    likelihood = likelihood, debug = debug, pp = pp
  )

  inits <- enw_inits(stan_data)

  fit <- enw_fit(data = stan_data, model = model, inits = inits, ...)

  out <- preprocessed_obs[, `:=`(
    stan_data = list(stan_data),
    inits = list(inits),
    fit = list(fit)
  )]

  class(out) <- c("epinowcast", class(out))
  return(out[])
}
