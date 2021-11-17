tar_target(summarised_7day_nowcast, {
  combined_nowcasts[, rbindlist(seven_day), by = c("model", "nowcast_date")]
})
