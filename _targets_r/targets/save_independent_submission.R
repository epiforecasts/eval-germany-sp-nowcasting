tar_target(
  save_independent_submission,
  save_csv(
    independent_submission_nowcast,
    filename = paste0(nowcast_dates, ".csv"),
    path = here("data/nowcasts/submission/independent")
  ),
  map(nowcast_dates),
  format = "file"
)
