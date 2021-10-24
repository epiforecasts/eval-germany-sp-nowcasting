enw_intercept_model <- function(pobs) {
  return(list(design = NULL, design_sd = NULL))
}

enw_random_intercept_model <- function(pobs) {
  # extract metadata about reported snapshots
  metaobs <- pobs$metadate[[1]]

  # turn dates into factors
  metaobs <- enw_dates_to_factors(metaobs)

  # build effects design matrix (with  no contrasts)
  design <- enw_design(~test, metaobs, no_contrasts = TRUE)

  # extract effects metadata
  effects <- enw_effects_metadata(design)

  # construct random effect for date
  effects <- enw_add_pooling_effect(effects, "date")

  # build design matrix for pooled parameters
  design_sd <- enw_design(~ fixed + sd, effects)

  return(list(design = design, design_sd = design_sd))
}

enw_day_of_week_model <- function(obs) {

}

enw_weekly_model <- function(obs, day_of_week = FALSE, rw = FALSE) {

}
