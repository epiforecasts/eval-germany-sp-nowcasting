independent_epinowcast <- function(obs, max_delay = 40, ...) {
  pobs_ind <- enw_preprocess_data(obs, max_delay = max_delay)

  metareference <- enw_add_cumulative_membership(
    pobs_ind$metareference[[1]],
    feature = "week"
  )

  reference_effects <- enw_formula(metareference, custom_random = "cweek")
  report_effects <- enw_formula(pobs_ind$metareport, random = "day_of_week")

  nowcast <- epinowcast(
    pobs_ind,
    reference_effects = reference_effects,
    report_effects = report_effects,
    ...
  )

  summary <- summary(
    nowcast,
    type = "nowcast",
    probs = c(0.025, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.975)
  )

  samples <- summary(nowcast, type = "nowcast_samples")

  out <- data.table::data.table(
    model = "Independent by age, Reference: Week, Report: Day of week",
    nowcast_date = max(summary$reference_date),
    summary = list(summary),
    samples = list(samples)
  )
  return(out)
}
