tar_target(summarised_nowcast, {
  unnest_nowcasts(combined_nowcasts, "daily")
})
