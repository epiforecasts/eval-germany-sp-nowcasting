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

enw_stan_data <- function(pobs,
                          date_effects = enw_intercept_model(
                            pobs$metadate[[1]]
                          ),
                          dist = "lognormal",
                          likelihood = TRUE, debug = FALSE,
                          nowcast = TRUE, pp = FALSE) {
  if (pp) {
    nowcast <- TRUE
  }

  # check dist type is supported and change to numeric
  dist <- match.arg(dist, c("lognormal", "gamma"))
  dist <- data.table::fcase(
    dist %in% "lognormal", 0,
    dist %in% "gamma", 1
  )
  # format latest matrix
  latest_matrix <- pobs$latest[[1]]
  latest_matrix <- data.table::dcast(
    latest_matrix, date ~ group,
    value.var = "confirm"
  )
  latest_matrix <- as.matrix(latest_matrix[, -1])

  # format vector of snapshot lengths
  snap_length <- pobs$new_confirm[[1]]
  snap_length <- snap_length[, .SD[delay == max(delay)],
    by = c("date", "group")
  ]
  snap_length <- snap_length$delay + 1

  # snap lookup
  snap_lookup <- unique(pobs$new_confirm[[1]][, .(date, group)])
  snap_lookup[, s := 1:.N]
  snap_lookup <- data.table::dcast(
    snap_lookup, date ~ group,
    value.var = "s"
  )
  snap_lookup <- as.matrix(snap_lookup[, -1])

  # snap time
  snap_time <- unique(pobs$new_confirm[[1]][, .(date, group)])
  snap_time[, t := 1:.N, by = "group"]
  snap_time <- snap_time$t

  # convert to stan list
  data <- list(
    t = pobs$time[[1]],
    s = pobs$snapshots[[1]],
    g = pobs$groups[[1]],
    st = snap_time,
    ts = snap_lookup,
    sl = snap_length,
    sg = unique(pobs$new_confirm[[1]][, .(date, group)])$group,
    dmax = pobs$max_delay[[1]],
    obs = as.matrix(pobs$reporting_triangle[[1]][, -c(1:2)]),
    latest_obs = latest_matrix,
    npmfs = nrow(date_effects$fixed$design),
    dpmfs = date_effects$fixed$index,
    neffs = ncol(date_effects$fixed$design) - 1,
    d_fixed = date_effects$fixed$design,
    neff_sds = ncol(date_effects$random$design) - 1,
    d_random = date_effects$random$design,
    dist = dist,
    debug = as.numeric(debug),
    likelihood = as.numeric(likelihood),
    pp = as.numeric(pp),
    cast = as.numeric(nowcast)
  )

  return(data)
}

enw_inits <- function(data) {
  init_fn <- function() {
    init <- list(
      logmean_int = rnorm(1, 1, 0.1),
      logsd_int = abs(rnorm(1, 0.5, 0.1)),
      uobs_logsd = array(abs(rnorm(data$g, 0, 0.1))),
      sqrt_phi = abs(rnorm(1, 0, 0.1))
    )
    init$logmean <- rep(init$logmean_int, data$npmfs)
    init$logsd <- rep(init$logsd_int, data$npmfs)
    init$phi <- 1 / sqrt(init$sqrt_phi)

    if (data$neffs > 0) {
      data$logmean_eff <- rnorm(data$neffs, 0, 0.01)
      data$logsd_eff <- rnorm(data$neffs, 0, 0.01)
    }
    return(init)
  }
  return(init_fn)
}
