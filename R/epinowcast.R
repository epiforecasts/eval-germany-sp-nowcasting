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
                       model = NULL, CrIs = c(0.2, 0.5, 0.9),
                       dist = "lognormal",
                       design = NULL, design_sd = NULL,
                       likelihood = TRUE, debug = FALSE,
                       ...) {
  stan_data <- enw_stan_data(preprocessed_obs,
    max_truncation = max_truncation,
    dist = dist,
    date_design = date_design,
    likelihood = likelihood, debug = debug
  )

  inits <- enw_inits(stan_data)

  fit <- enw_fit(data = stan_data, model = model, inits = inits, ...)

  out <- data.table::data.table(
    stan_data = list(stan_data),
    inits = list(inits),
    fit = list(fit)
  )

  class(out) <- c("epinowcast", class(out))
  return(out[])
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

plot_epinowcast <- function(posterior, obs, log = FALSE) {
  if (missing(obs)) {
    obs <- NULL
  }

  if (!is.null(obs)) {
    posterior <- data.table::copy(posterior)
    obs <- data.table::copy(obs)
    posterior <- merge(
      posterior[, last_confirm := NULL],
      obs[, last_confirm := confirm][, confirm := NULL],
      by = "date", all.x = TRUE
    )
  }

  plot <- ggplot2::ggplot(posterior) +
    ggplot2::aes(x = date, y = last_confirm) +
    ggplot2::geom_point(
      show.legend = FALSE, na.rm = TRUE, alpha = 0.7, shape = 2
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = date, y = confirm),
      shape = 1, alpha = 0.7
    )

  plot <- plot_CrIs(plot, extract_CrIs(posterior),
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
    est <- data.table::rbindlist(x$nowcast)
  } else if (type %in% "posterior") {
    est <- data.table::rbindlist(x$posterior_prediction)
  }

  if (missing(obs)) {
    obs <- NULL
  }

  if (!missing(report_dates)) {
    est <- est[report_date %in% as.Date(report_dates)]
  }

  plot <- plot_epinowcast(est, obs = obs, log = log)

  if (length(unique(est$report_date)) > 1) {
    plot <- plot +
      ggplot2::facet_wrap(~report_date, scales = "free")
  }
  return(plot)
}
