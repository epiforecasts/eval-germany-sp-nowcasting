plot_nowcasts_at_horizon <- function(summarised_nowcast, latest_obs,
                                     max_delay = 28, ...) {
   epinowcast::enw_plot_nowcast_quantiles(
      summarised_nowcast[
        reference_date < (max(nowcast_date) - ma_delay)][,
        c("holiday", "confirm") := NULL][,
        horizon := as.numeric(as.Date(reference_date) - nowcast_date)],
      latest_obs = latest_obs[
        reference_date >= min(summarised_nowcast$reference_date)][
        reference_date <= max(summarised_nowcast$reference_date)
      ],
      log = TRUE, ...
    )
}