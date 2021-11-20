tar_target(summarised_7day_nowcast, {
  combined_nowcasts |> 
    adjust_posterior(
      target = "seven_day", 
      max_scale = 0.5, 
      condition = "max_rhat > 1.1 | per_divergent_transitions > 0.2"
  ) |> 
    unnest_nowcasts(target = "seven_day") 
})
