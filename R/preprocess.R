enw_metadata <- function(obs, date_to_drop = c("report_date")) {
  date_to_drop <- match.arg(date_to_drop, c("date", "report_date"))
  metaobs <- data.table::as.data.table(obs)
  metaobs[, c(date_to_drop, "confirm") := NULL]
  metaobs <- unique(metaobs)
  return(metaobs[])
}

is.Date <- function(x) { # nolint
  inherits(x, "Date")
}

enw_dates_to_factors <- function(data) {
  data <- data.table::as.data.table(data)
  cols <- colnames(data)[sapply(data, is.Date)]
  data <- data[, lapply(.SD, factor), .SDcols = cols]
  return(data[])
}

enw_design <- function(formula, data, no_contrasts = FALSE, ...) {
  # make data.table and copy
  data <- data.table::as.data.table(data)

  # make no  intercept model.matrix
  mod_matrix <- function(formula, data, ...) {
    design <- model.matrix(formula, data, ...)
    design <- design[, !(colnames(design) %in% "(Intercept)")]
  }

  # design matrix using default contrasts
  if (length(no_contrasts) == 1 && !no_contrasts) {
    design <- mod_matrix(formula, data, ...)
    return(design)
  } else {
    if (length(no_contrasts) == 1 && no_contrasts) {
      no_contrasts <- colnames(data)[
        sapply(data, function(x) is.factor(x) | is.character(x))
      ]
    }
    # what is in the formula
    in_form <- rownames(attr(stats::terms(formula, data = data), "factors"))

    # drop contrasts not in the formula
    no_contrasts <- no_contrasts[no_contrasts %in% in_form]

    if (length(no_contrasts) == 0) {
      design <- mod_matrix(formula, data, ...)
      return(design)
    } else {
      # check everything is  a factor that should be
      data[, lapply(.SD, as.factor), .SDcols = no_contrasts]

      # make list of contrast args
      contrast_args <- purrr::map(
        no_contrasts, ~ stats::contrasts(data[[.]], contrast = FALSE)
      )
      names(contrast_args) <- no_contrasts

      # model matrix with contrast options
      design <- mod_matrix(formula, data, contrasts.arg = contrast_args, ...)
      return(design)
    }
  }
}

enw_effects_metadata <- function(design) {
  dt <- data.table::data.table(effects = colnames(design), fixed = 1)
  dt <- dt[!effects %in% "(Intercept)"]
  return(dt[])
}

enw_add_pooling_effect <- function(effects, string) {
  effects[, sd := ifelse(grepl(string, effects), 1, 0)]
  effects[grepl("report_date", effects), fixed := 0]
  return(effects[])
}

enw_assign_group <- function(obs, by = c()) {
  obs <- data.table::as.data.table(obs)
  if (length(by) == 0) {
    obs <- obs[, group := 1]
  } else {
    groups_index <- data.table::copy(obs)
    groups_index <- unique(groups_index[, ..by])
    groups_index[, group := 1:.N]
    obs <- merge(obs, groups_index, by = by, all.x = TRUE)
  }
  return(obs = obs[])
}

enw_latest_data <- function(obs) {
  latest_data <- data.table::copy(obs)[,
    .SD[report_date == max(report_date)],
    by = c("date", "group")
  ]
  latest_data[, report_date := NULL]
  return(latest_data[])
}

enw_new_reports <- function(obs, max_delay = 20, min_report_date = NULL) {
  reports <- data.table::copy(obs)
  reports <- reports[order(date)]
  reports[, new_confirm := confirm - data.table::shift(confirm, fill = 0),
    by = c("date", "group")
  ]
  reports <- reports[, .SD[date >= min(report_date)],
    by = c("group")
  ]
  reports <- reports[, delay := 0:(.N - 1), by = c("date", "group")]
  reports <- reports[delay < max_delay]
  return(reports[])
}

enw_reporting_triangle <- function(obs) {
  obs <- data.table::as.data.table(obs)
  if (any(obs$new_confirm < 0)) {
    stop("Negative new confirmed cases found. This is not yet supported.")
  }
  reports <- data.table::dcast(
    obs, group + date ~ delay,
    value.var = "new_confirm", fill = 0
  )
  return(reports[])
}

enw_reporting_triangle_to_long <- function(obs) {
  reports_long <- data.table::melt(
    obs,
    id.vars = c("group", "date"),
    variable.name = "delay", value.name = "new_confirm"
  )
  data.table::setorderv(reports_long, c("group", "date"))
  return(reports_long[])
}

enw_preprocess_data <- function(obs, by = c(), max_delay = 20,
                                min_report_date, set_negatives_to_zero = TRUE) {
  obs <- data.table::as.data.table(obs)
  obs <- obs[order(date)]

  if (!missing(min_report_date)) {
    obs <- obs[report_date >= min_report_date]
  }

  # assign groups
  obs <- enw_assign_group(obs, by = by)

  # difference reports and filter for max delay an report date
  diff_obs <- enw_new_reports(obs, max_delay = max_delay)

  if (set_negatives_to_zero) {
    diff_obs <- diff_obs[new_confirm < 0, new_confirm := 0]
  }

  # filter obs based on diff constraints
  obs <- merge(obs, diff_obs[, .(date, report_date, group)],
    by = c("date", "report_date", "group")
  )

  # update grouping in case any are now missing
  obs <- enw_assign_group(obs[, old_group := group], by)

  # update diff data groups using updated groups
  diff_obs <- merge(
    diff_obs,
    obs[, .(date, report_date, new_group = group, group = old_group)],
    by = c("date", "report_date", "group")
  )
  diff_obs[, group := new_group][, new_group := NULL]
  obs[, old_group := NULL]

  # calculate reporting matrix
  reporting_triangle <- enw_reporting_triangle(diff_obs)

  # extract latest data
  latest <- enw_latest_data(obs)

  out <- data.table::data.table(
    obs = list(obs),
    new_confirm = list(diff_obs),
    latest = list(latest),
    reporting_triangle = list(reporting_triangle),
    metadate = list(enw_metadata(obs)),
    metareport = list(enw_metadata(obs, date_to_drop = "date")),
    time = nrow(latest),
    snapshots = nrow(unique(obs[, .(group, report_date)])),
    groups = length(unique(obs$group)),
    max_delay = max_delay
  )
  return(out[])
}
