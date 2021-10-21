#' Extract a Parameter Summary from a Stan Object
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts summarised parameter posteriors from a `stanfit` object using
#' `rstan::summary` in a format consistent with other summary functions in
#' `EpiNow2`.
#' @param fit A `stanfit` object
#' @param params A character vector of parameters to extract. Defaults to all parameters.
#' @param var_names Logical defaults to `FALSE`. Should variables be named. Automatically set
#' to TRUE if multiple parameters are to be extracted.
#' @return A `data.table` summarising parameter posteriors. Contains a following variables:
#' `variable`, `mean`, `mean_se`, `sd`, `median`, and `lower_`, `upper_` followed by
#' credible interval labels indicating the credible intervals present.
#' @inheritParams calc_summary_measures
#' @export
#' @importFrom data.table as.data.table :=
#' @importFrom rstan summary
extract_stan_param <- function(fit, params = NULL,
                               CrIs = c(0.2, 0.5, 0.9), var_names = FALSE) {
  # generate symmetric CrIs
  CrIs <- CrIs[order(CrIs)]
  sym_CrIs <- c(0.5, 0.5 - CrIs / 2, 0.5 + CrIs / 2)
  sym_CrIs <- sym_CrIs[order(sym_CrIs)]
  CrIs <- round(100 * CrIs, 0)
  CrIs <- c(paste0("lower_", rev(CrIs)), "median", paste0("upper_", CrIs))
  args <- list(object = fit, probs = sym_CrIs)
  if (!is.null(params)) {
    if (length(params) > 1) {
      var_names <- TRUE
    }
    args <- c(args, pars = params)
  } else {
    var_names <- TRUE
  }
  summary <- do.call(rstan::summary, args)
  summary <- data.table::as.data.table(summary$summary,
    keep.rownames = ifelse(var_names,
      "variable",
      FALSE
    )
  )
  cols <- c("mean", "se_mean", "sd", CrIs, "n_eff", "Rhat")
  if (var_names) {
    cols <- c("variable", cols)
  }
  colnames(summary) <- cols
  summary <- summary[, c("n_eff", "Rhat") := NULL]
  return(summary)
}



enw_metadata <- function(obs) {
  metaobs <- data.table::as.data.table(obs)
  metaobs[, c("date", "confirm") := NULL]
  metaobs <- unique(metaobs)

  if (dates_to_factors) {
    cols <- sapply(metaobs, is.Date)
  }
  return(metaobs)
}

enw_dates_to_factors <- function(data) {
  data <- data.table::as.data.table(data)
  cols <- colnames(data)[sapply(data, is.Date)]
  data <- data[, lapply(.SD, factor), .SDcols = cols]
  return(data)
}

enw_no_contrast_factor <- function(factor, data) {
  list(factor = contrasts(data[[factor]], contrasts = FALSE))
}
enw_design <- function(formula, data, ...) {
  design <- model.matrix(formula, data, ...)
  design <- design[, !(colnames(design) %in% "(Intercept)")]
  return(design)
}

is.Date <- function(x) {
  inherits(x, "Date")
}

enw_effects_metadata <- function(design) {
  dt <- data.table::data.table(effects = colnames(tmp), fixed = 1)
  dt <- dt[!effects %in% "(Intercept)"]
  return(dt)
}

enw_data <- function(obs, design = NULL, design_sd = NULL,
                     max_truncation = 20, likelihood = TRUE,
                     debug = TRUE) {
  dirty_obs <- data.table::as.data.tabls(obs)
  dirty_obs <- dirty_obs[order(report_date)]
  obs <- data.table::copy(dirty_obs)
  obs <- split(obs, by = "report_date")
  obs <- purrr::map(1:length(obs), ~ obs[[.]][, .(date, confirm)])
  obs <- purrr::map(
    1:length(obs),
    ~ data.table::setnames(
      obs[[.]],
      "confirm", as.character(.)
    )
  )
  obs <- purrr::reduce(obs, merge, all = TRUE)
  obs_start <- nrow(obs) - max_truncation - sum(is.na(obs$`1`)) + 1
  tdist <- purrr::map_dbl(2:(ncol(obs)), ~ sum(is.na(obs[[.]])))
  obs_data <- obs[, -1][, purrr::map(.SD, ~ ifelse(is.na(.), 0, .))]
  obs_data <- obs_data[obs_start:.N]

  latest_obs <- dirty_obs[report_date == max(report_date)]
  data.table::setnames(latest_obs, "confirm", "last_confirm")

  # specify design matrix if missing
  if (is.null(design)) {
    design <- matrix(1, nrow = ncol(obs_data), ncol = 1)
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
  # convert to stan list
  data <- list(
    obs = obs_data,
    tdist = tdist,
    t = nrow(obs_data),
    nobs = ncol(obs_data),
    tmax = max_truncation,
    neffs = neffs,
    neff_sds = neff_sds,
    design = design,
    design_sd = design_sd,
    debug = as.numeric(debug),
    likelihood = as.numeric(likelihood)
  )

  out <- list(
    dirty = dirty_obs,
    latest = latest_obs,
    report_matrix = obs,
    stan = data
  )
  return(out)
}

enw_inits <- function(data) {
  init_fn <- function() {
    init <- list(
      logmean_init = rnorm(1, 0, 1),
      logsd_init = abs(rnorm(1, 0, 1)),
      uobs_logsd = abs(rnorm(1, 0, 0.1)),
      log_uobs_resids = rnorm(data$tmax, 0, 1),
      phi = abs(rnorm(1, 0, 0.1))
    )
    init$logmean <- rep(init$logmean_init, data$nobs)
    init$logsd <- rep(init$logsd_init, data$nobs)
    init$sqrt_phi <- 1 / sqrt(init$phi)
    return(init)
  }
  return(init_fn)
}

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

link_obs <- function(index, obs, dirty, last, max_truncation) {
  target_obs <- data.table::copy(dirty[[index]])[, index := .N - 0:(.N - 1)]
  target_obs <- target_obs[index <= max_truncation]
  estimates <- obs[dataset == index][, c("id", "dataset") := NULL]
  estimates <- estimates[, index := .N - 0:(.N - 1)]
  target_obs <-
    data.table::merge.data.table(
      target_obs, data.table::copy(last)[, report_date := NULL],
      by = "date"
    )
  target_obs <- data.table::merge.data.table(target_obs, estimates,
    by = "index", all.x = TRUE
  )
  target_obs <- target_obs[order(date)][, index := NULL]
  return(target_obs)
}

enw_imputed_obs <- function(fit, data, CrIs) {
  imp <- extract_stan_param(fit, "sim_imputed_obs",
    CrIs = CrIs,
    var_names = TRUE
  )
  imp[, variable := NULL]
  obs <- data.table::copy(data$dirty)
  obs <- obs[, last_confirm := confirm]
  obs <- obs[(.N - data$stan$tmax + 1):.N]

  imp <- cbind(obs, imp)
  return(imp)
}

enw_posterior_predictions <- function(fit, target, data, CrIs,
                                      max_truncation) {
  datasets <- data$stan$nobs
  dirty <- split(data$dirty, by = "report_date")
  last <- data$latest

  obs <- extract_stan_param(fit, target,
    CrIs = CrIs,
    var_names = TRUE
  )

  # assign labels of interest
  obs[, id := variable][, variable := NULL]
  obs[, dataset := 1:.N]
  obs[, dataset := dataset %% datasets]
  obs <- obs[dataset == 0, dataset := datasets]

  tidy_out <- purrr::map(
    1:(datasets), link_obs,
    obs = obs,
    dirty = dirty, last = last,
    max_truncation = max_truncation
  )
  tidy_out <- data.table::rbindlist(tidy_out)
  return(tidy_out)
}

truncation_dist <- function(fit, truncation_max) {
  list(
    mean = round(rstan::summary(fit, pars = "logmean")$summary[1], 3),
    mean_sd = round(rstan::summary(fit, pars = "logmean")$summary[3], 3),
    sd = round(rstan::summary(fit, pars = "logsd")$summary[1], 3),
    sd_sd = round(rstan::summary(fit, pars = "logsd")$summary[3], 3),
    max = truncation_max
  )
}

truncation_cmf <- function(fit, CrIs) {
  cmf <- extract_stan_param(fit, "cmf", CrIs = CrIs)
  cmf <- data.table::as.data.table(cmf)[, index := .N:1]
  data.table::setcolorder(cmf, "index")
  return(cmf)
}

#' Nowcast of Observed Data
#'
#' @description `r lifecycle::badge("experimental")`
#' Estimates a truncation distribution from multiple snapshots of the same
#' data source over time. This distribution can then be used in `regional_epinow`,
#' `epinow`, and `estimate_infections` to adjust for truncated data (i.e to
#' nowcast). See [here](https://gist.github.com/seabbs/176b0c7f83eab1a7192a25b28bbd116a)
#' for an example of using this approach on Covid-19 data in England.
#'
#' @param obs A data.frames containing a date variable, a confirm (integer)
#' variable and a report_date (date of report) variable. Stratifying by report
#' date should yield notifications as reported on that day with no missing
#' dates.
#'
#' @param max_truncation Integer, defaults to 10. Maximum number of
#' days to include in the truncation distribution.
#'
#' @param model A compiled stan model to override the default model. May be
#' useful for package developers or those developing extensions.
#'
#' @param ... Additional parameters to pass to `rstan::sampling`.
#'
#' @return A list containing: the summary parameters of the truncation distribution
#'  (`dist`), the estimated CMF of the truncation distribution (`cmf`, can be used to adjusted
#'  new data), a data frame containing the observed truncated data, latest observed data
#'  and the adjusted for truncation observations (`obs`), a data frame containing the last
#'  observed data (`last_obs`, useful for plotting and validation), the data used for fitting
#'  (`data`) and the fit object (`fit`).
#' @export
#' @inheritParams calc_CrIs
#' @importFrom purrr map reduce map_dbl
#' @importFrom rstan sampling
#' @importFrom data.table copy .N as.data.table merge.data.table setDT setcolorder
#' @examples
#' # set number of cores to use
#' options(mc.cores = ifelse(interactive(), 4, 1))
#' # get example case counts
#' reported_cases <- example_confirmed[1:60]
#'
#' # define example truncation distribution (note not integer adjusted)
#' trunc_dist <- list(
#'   mean = convert_to_logmean(3, 2),
#'   mean_sd = 0.1,
#'   sd = convert_to_logsd(3, 2),
#'   sd_sd = 0.1,
#'   max = 10
#' )
#'
#' # apply truncation to example data
#' construct_truncation <- function(index, cases, dist) {
#'   set.seed(index)
#'   cmf <- cumsum(
#'     dlnorm(
#'       1:(dist$max + 1),
#'       rnorm(1, dist$mean, dist$mean_sd),
#'       rnorm(1, dist$sd, dist$sd_sd)
#'     )
#'   )
#'   cmf <- cmf / cmf[dist$max + 1]
#'   cmf <- rev(cmf)[-1]
#'   trunc_cases <- data.table::copy(cases)[1:(.N - index)]
#'   trunc_cases[(.N - length(cmf) + 1):.N, confirm := as.integer(confirm * cmf)]
#'   return(trunc_cases)
#' }
#' example_data <- purrr::map(c(20, 15, 10, 0),
#'   construct_truncation,
#'   cases = reported_cases,
#'   dist = trunc_dist
#' )
#'
#' # fit model to example data
#' est <- nowcast(example_data,
#'   verbose = interactive(),
#'   chains = 2, iter = 2000
#' )
#'
#' # summary of the distribution
#' est$dist
#' # summary of the estimated truncation cmf (can be applied to new data)
#' print(est$cmf)
#' # observations linked to truncation adjusted estimates
#' print(est$obs)
#' # validation plot of observations vs estimates
#' plot(est)
epinowcast <- function(obs, max_truncation = 10,
                       model = NULL, CrIs = c(0.2, 0.5, 0.9),
                       likelihood = TRUE, debug = TRUE,
                       ...) {
  data <- enw_data(obs,
    max_truncation = max_truncation,
    likelihood = likelihood, debug = debug
  )
  out <- data

  # initial conditions
  inits <- enw_inits(data$stan)

  # fit
  fit <- enw_fit(data = data$stan, model = model, inits = inits, ...)

  # Summarise fit truncation distribution for downstream usage
  # out$dist <- truncation_dist(fit, max_truncation)

  # summarise nowcast for target dataset
  out$nowcast <- enw_imputed_obs(
    fit, data, CrIs
  )

  # summarse simulated truncated observations for all datasets
  out$posterior_prediction <- enw_posterior_predictions(
    fit, "sim_trunc_obs", data, CrIs, max_truncation
  )

  # summarise estimated cmf of the truncation distribution
  # out$cmf <- truncation_cmf(fit, CrIs)
  out$fit <- fit

  class(out) <- c("epinowcast", class(out))
  return(out)
}

#' Extract Credible Intervals Present
#'
#' @description `r lifecycle::badge("stable")`
#' Helper function to extract the credible intervals present in a data frame.
#' @param summarised A data frame as processed by `calc_CrIs`
#' @return A numeric vector of credible intervals detected in the data frame.
#' @export
#' @examples
#' samples <- data.frame(value = 1:10, type = "car")
#' summarised <- calc_CrIs(samples,
#'   summarise_by = "type",
#'   CrIs = c(seq(0.05, 0.95, 0.05))
#' )
#' extract_CrIs(summarised)
extract_CrIs <- function(summarised) {
  CrIs <- grep("lower_", colnames(summarised), value = TRUE)
  CrIs <- gsub("lower_", "", CrIs)
  CrIs <- as.numeric(CrIs)
  return(CrIs)
}

#' Plot EpiNow2 Credible Intervals
#'
#' @description `r lifecycle::badge("stable")`
#' Adds lineranges for user specified credible intervals
#' @param plot A `ggplot2` plot
#' @param CrIs Numeric list of credible intervals present in the data. As produced
#' by `extract_CrIs`
#' @param alpha Numeric, overall alpha of the target line range
#' @param size Numeric, size of the default line range.
#' @return A `ggplot2` plot.
plot_CrIs <- function(plot, CrIs, alpha, size) {
  index <- 1
  alpha_per_CrI <- alpha / (length(CrIs) - 1)
  for (CrI in CrIs) {
    bottom <- paste0("lower_", CrI)
    top <- paste0("upper_", CrI)
    if (index == 1) {
      plot <- plot +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]),
          alpha = 0.2, size = size
        )
    } else {
      plot <- plot +
        ggplot2::geom_ribbon(ggplot2::aes(
          ymin = .data[[bottom]], ymax = .data[[top]],
          col = NULL
        ),
        alpha = alpha_per_CrI
        )
    }
    index <- index + 1
  }
  return(plot)
}

#' Plot method for nowcast
#'
#' @description
#' `plot` method for class "nowcast". Returns
#' a plot faceted over each dataset used in fitting with the latest
#' observations as columns, the data observed at the time (and so truncated)
#' as dots and the truncation adjusted estimates as a ribbon.
#'
#' @param x A list of output as produced by `nowcast`
#'
#' @param type A character string indicating the type of plot required.
#' Currently supported options are "nowcast" which plots the nowcast
#' for each dataset along with the latest available observed data, and
#' "posterior" which plots observations reported at the time against
#' simulated observations from  the model.
#'
#' @param log Logical, defaults to `FALSE`. Should cases be plotted on a
#' log scale.
#'
#' @param obs Optional data.frame of observations to plot.
#' Must have date and confirm variables. If not supplied then the lastest
#' available observations from the data used to fit the truncation are used.
#'
#' @param report_dates A vector of dates to plot. If not supplied all available
#' dates are plot.
#'
#' @param ... Pass additional arguments to plot function. Not currently in use.
#'
#' @seealso plot epinowcast
#' @method plot epinowcast
#' @return `ggplot2` object
#' @importFrom ggplot2 ggplot aes geom_col geom_point labs scale_x_date scale_y_continuous theme
#' @export
plot.epinowcast <- function(x, type = "nowcast", log = FALSE,
                            obs, report_dates, ...) {
  type <- match.arg(
    type,
    choices = c("nowcast", "posterior")
  )
  if (type %in% "nowcast") {
    est <- x$nowcast
  } else if (type %in% "posterior") {
    est <- x$posterior_prediction
  }

  if (!missing(obs)) {
    est <- data.table::copy(est)
    obs <- data.table::copy(obs)
    est <- merge(
      est[, last_confirm := NULL],
      obs[, last_confirm := confirm][, confirm := NULL],
      by = "date", all.x = TRUE
    )
  }

  if (!missing(report_dates)) {
    est <- est[report_date %in% as.Date(report_dates)]
  }

  plot <- ggplot2::ggplot(est) +
    ggplot2::aes(x = date, y = last_confirm) +
    ggplot2::geom_col(
      fill = "grey", col = "white",
      show.legend = FALSE, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = date, y = confirm)
    )

  if (length(unique(est$report_date)) > 1) {
    plot <- plot +
      ggplot2::facet_wrap(~report_date, scales = "free")
  }

  plot <- plot_CrIs(plot, extract_CrIs(est),
    alpha = 0.8, size = 1
  )

  plot <- plot +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "Confirmed Cases", x = "Date", col = "Type", fill = "Type") +
    ggplot2::scale_x_date(date_breaks = "day", date_labels = "%b %d") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

  if (log) {
    plot <- plot + ggplot2::scale_y_log10(labels = scales::comma)
  } else {
    plot <- plot + ggplot2::scale_y_continuous(labels = scales::comma)
  }
  return(plot)
}
