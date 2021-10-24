enw_intercept_model <- function(obs) {
  return(list(design = NULL, design_sd = NULL))
}

enw_random_intercept_model <- function(obs) {
  # extract metadata about reported snapshots
  metaobs <- enw_metadata(obs)

  # turn dates into factors
  metaobs <- enw_dates_to_factors(metaobs)

  # build effects design matrix (with  no contrasts)
  design <- enw_design(~test, metaobs, no_contrasts = TRUE)

  # extract effects metadata
  effects <- enw_effects_metadata(design)

  # construct random effect for report_date
  effects <- enw_add_pooling_effect(effects, "report_date")

  # build design matrix for pooled parameters
  design_sd <- enw_design(~ fixed + sd, effects)

  return(list(design = design, design_sd = design_sd))
}

enw_day_of_week_model <- function(obs) {

}

enw_weekly_model <- function(obs, day_of_week = FALSE, rw = FALSE) {

}
