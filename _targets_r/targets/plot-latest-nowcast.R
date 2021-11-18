tar_target(
  plot_latest_nowcast,
  enw_plot_nowcast_quantiles(
    summarised_nowcasts[nowcast_date == max(nowcast_date)][
                        location == locations][
                        reference_date >= (nowcast_date - 28)], 
    latest_obs = latest_hospitalisations[location == locations][
                                         reference_date >= (max(report_date) - 
                                                             40)]
  ) +
  facet_grid(vars(age_group), vars(model), scales = "free_y"),
  map(locations)
)
