enw_default_design <- function(design, rows) {
  if (is.null(design[[1]])) {
    design[[1]] <- matrix(1, nrow = rows, ncol = 1)
    neffs <- 0
  } else {
    neffs <- ncol(design)
  }
  if (is.null(design_sd)) {
    design_sd <- matrix(1, nrow = neffs, ncol = 1)
    neff_sds <- 0
  } else {
    neff_sds <- ncol(design_sd) - 1
  }
  stopifnot(
    "Number of design matrix columns must equal design_sd rows" = neffs == nrow(design_sd) # nolint
  )
}

enw_stan_data <- function(preprocessed_obs, date_design = list(NULL, NULL),
                          dist = "lognormal",
                          likelihood = TRUE, debug = FALSE) {

  # specify design matrix if missing
  date_design <- enw_default_design(date_design)

  # check dist type is supported
  dist <- match.arg(dist, c("lognormal", "gamma"))
  dist <- data.table::fcase(
    dist %in% "lognormal", 0,
    dist %in% "gamma", 1
  )
  # convert to stan list
  data <- list(
    obs = obs_data,
    diff_obs = diff_obs,
    tdist = tdist,
    t = nrow(obs_data),
    nobs = ncol(obs_data),
    tmax = max_delay,
    neffs = neffs,
    neff_sds = neff_sds,
    design = date_design[[1]],
    design_sd = date_design[[2]],
    dist = dist,
    debug = as.numeric(debug),
    likelihood = as.numeric(likelihood)
  )

  return(out)
}


enw_inits <- function(data) {
  init_fn <- function() {
    init <- list(
      logmean_init = rnorm(1, 1, 0.1),
      logsd_init = abs(rnorm(1, 0.5, 0.1)),
      uobs_logsd = abs(rnorm(1, 0, 0.1)),
      log_uobs_resids = rnorm(data$tmax, 0, 1),
      sqrt_phi = abs(rnorm(1, 0, 0.1))
    )
    init$logmean <- rep(init$logmean_init, data$nobs)
    init$logsd <- rep(init$logsd_init, data$nobs)
    init$phi <- 1 / sqrt(init$sqrt_phi)

    if (data$neffs > 0) {
      data$logmean_eff <- rnorm(data$neffs, 0, 0.01)
      data$logsd_eff <- rnorm(data$neffs, 0, 0.01)
    }
    return(init)
  }
  return(init_fn)
}
