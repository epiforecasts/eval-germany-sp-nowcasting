tar_target(
  independent_submission_nowcast,
  rbind(
    independent_nowcast[nowcast_date == nowcast_dates],
    overall_only_nowcast[nowcast_date == nowcast_dates],
  ) |> 
   adjust_posterior(
      target = "seven_day", 
      max_scale = 0.5, 
      condition = "max_rhat > 1.1 | per_divergent_transitions > 0.2"
    ) |>
    select_var("seven_day") |>
    rbindlist() |>
    format_for_submission(),
  map(nowcast_dates),
  iteration = "list"
)
