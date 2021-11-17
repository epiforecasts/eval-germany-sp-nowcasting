tar_target(
  save_daily_nowcasts,
  summarise_nowcast[nowcast_date == nowcast_dates] |>
    save_csv(
      filename = paste0(nowcast_dates, ".csv"),
      path = here("data/nowcasts/daily")
    ),
  map(nowcast_dates),
  format = "file"
)
