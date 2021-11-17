tar_target(
  fixed_nowcast,
  nowcast(
    obs = hospitalisations_by_date_report,
    tar_loc = locations, tar_date = nowcast_dates,
    model = fixed_epinowcast, model_file = epinowcast_model,
    max_delay = max_report_delay, settings = epinowcast_settings
  ),
  cross(nowcast_dates, locations)
)
