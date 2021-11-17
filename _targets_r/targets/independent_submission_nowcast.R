tar_target(
  independent_submission_nowcast,
  independent_nowcast[nowcast_date == nowcast_dates]$seven_day |>
    rbindlist() |>
    format_for_submission(),
  map(nowcast_dates),
  iteration = "list"
)
