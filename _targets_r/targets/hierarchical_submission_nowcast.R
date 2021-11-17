tar_target(
  hierarchical_submission_nowcast,
  combined_nowcasts[
    model == "Reference: Age and week by age, Report: Day of week"][
    nowcast_date == nowcast_dates
    ]$seven_day |>
    format_for_submission(),
  map(nowcast_dates),
  iteration = "list"
)
