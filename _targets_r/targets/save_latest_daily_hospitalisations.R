tar_target(
  save_latest_daily_hospitalisations,
  save_csv(
    latest_hospitalisations,
    filename = paste0("daily.csv"),
    path = here("data/observations")
  ),
  format = "file"
)
