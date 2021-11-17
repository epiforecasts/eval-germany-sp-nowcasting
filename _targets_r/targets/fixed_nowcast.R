tar_target(
  fixed_nowcast,
  do.call(
    fixed_epinowcast, c(
      list(
        obs = hospitalisations,
        max_delay = max_report_delay,
        model_file = epinowcast_model
      ),
      epinowcast_settings
    )
  ),
  cross(nowcast_dates, location)
)
