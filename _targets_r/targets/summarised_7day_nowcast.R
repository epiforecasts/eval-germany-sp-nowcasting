tar_target(summarised_7day_nowcast, {
  combined_nowcasts |> 
    adjust_posteriors(
      target = "seven_day", 
      max_scale = 0.5, 
      rhat_bound = 1.1,
      per_dt_bound = 0.2
  ) |> 
    unnest_nowcasts(target = "seven_day") 
})
