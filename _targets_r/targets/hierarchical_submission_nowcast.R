tar_target(
  hierarchical_submission_nowcast,
  age_week_nowcast[nowcast_date == nowcast_dates] |>
    adjust_posterior(
      target = "seven_day", 
      max_scale = 0.5, 
      condition = "max_rhat > 1.1 | per_divergent_transitions > 0.2"
    )$seven_day |>
    rbindlist() |>
    format_for_submission(),
  map(nowcast_dates),
  iteration = "list"
)
