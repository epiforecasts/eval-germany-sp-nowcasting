enw_intercept_model <- function(metaobs) {

  # build effects design matrix (with  no contrasts)
  fixed <- enw_design(~1, metaobs, sparse = TRUE)

  # extract effects metadata
  effects <- enw_effects_metadata(fixed$design)

  # construct random effect for date
  effects <- enw_add_pooling_effect(effects, "date")

  # build design matrix for pooled parameters
  random <- enw_design(~1, effects, sparse = FALSE)

  return(list(fixed = fixed, random = random))
}

enw_random_intercept_model <- function(metaobs) {

  # turn dates into factors
  metaobs <- enw_dates_to_factors(metaobs)

  # build effects design matrix (with  no contrasts)
  design <- enw_design(~"date", metaobs, no_contrasts = TRUE)

  # extract effects metadata
  effects <- enw_effects_metadata(design$design)

  # construct random effect for date
  effects <- enw_add_pooling_effect(effects, "date")

  # build design matrix for pooled parameters
  design_sd <- enw_design(~ 1 + sd, effects)

  return(list(fixed = fixed, random = random))
}

enw_day_of_week_model <- function(obs) {

}

enw_weekly_model <- function(obs, day_of_week = FALSE, rw = FALSE) {

}
