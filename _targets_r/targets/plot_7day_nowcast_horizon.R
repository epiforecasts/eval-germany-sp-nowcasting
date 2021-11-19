tar_map(
  list(horizons = 0:7),
  tar_target(
    plot_7day_nowcast_horizon,
    enw_plot_nowcast_quantiles(
      summarised_7day_nowcast[
        reference_date < (max(nowcast_date) - 28)][,
        holiday := NULL][,
        horizon := as.numeric(as.Date(reference_date) - nowcast_date)][
        location == locations][
        horizon == -horizons][,
        confirm := NA],
      latest_obs = latest_7day_hospitalisations[
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
