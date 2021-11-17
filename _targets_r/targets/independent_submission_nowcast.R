tar_target(
  independent_submission_nowcast,
  combined_nowcasts[
    model == "Independent by age, Reference: Week, Report: Day of week"][
    nowcast_date == nowcast_dates
    ]$seven_day |>
    format_for_submission(),
  map(nowcast_dates),
  iteration = "list"
)
