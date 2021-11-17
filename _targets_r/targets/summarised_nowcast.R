tar_target(summarised_nowcast, {
  combined_nowcasts[, rbindlist(daily), by = c("model", "nowcast_date")]
})
