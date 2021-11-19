tar_map(
  list(horizons = 0:7),
  tar_target(
    plot_nowcast_horizon,
    enw_plot_nowcast_quantiles(
      scored_nowcasts[location == locations][horizon == -horizons][,
                      confirm := NA],
      latest_obs = latest_hospitalisations[
        location == locations][
        reference_date >= min(scored_nowcasts$reference_date)][
        reference_date <= max(scored_nowcasts$reference_date)
      ],
      log = TRUE
    ) +
    facet_grid(vars(age_group), vars(model), scales = "free_y"),
    map(locations),
    iteration = "list"
  )
)
