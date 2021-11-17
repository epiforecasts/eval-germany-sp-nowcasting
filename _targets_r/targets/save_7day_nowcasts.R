tar_target(
  save_7day_nowcasts,
  summarised_7day_nowcast[nowcast_date == nowcast_dates] |>
    save_csv(
      filename = paste0(nowcast_dates, ".csv"),
      path = here("data/nowcasts/seven_day")
    ),
  map(nowcast_dates),
  format = "file"
)
