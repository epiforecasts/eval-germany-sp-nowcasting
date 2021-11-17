tar_target(
  hierarchical_submission_nowcast,
  age_week_nowcast[nowcast_date == nowcast_dates]$seven_day |>
    rbindlist() |>
    format_for_submission(),
  map(nowcast_dates),
  iteration = "list"
)
