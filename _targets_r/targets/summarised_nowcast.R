tar_target(summarised_nowcast, {
  combined_nowcasts |> 
    adjust_posterior(
      target = "daily", 
      max_scale = 0.5, 
      condition = "max_rhat > 1.1 | per_divergent_transitions > 0.2"
  ) |> 
    unnest_nowcasts(target = "daily") 
})
