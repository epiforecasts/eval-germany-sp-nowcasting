tar_target(summarised_7day_nowcast, {
  unnest_nowcasts(combined_nowcasts, "seven_day")
})
