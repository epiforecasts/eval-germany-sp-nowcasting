plot_nowcast <- function(summarised_nowcast, latest_obs,
                                     max_delay = Inf, ...) {
   summarised_nowcast <- summarised_nowcast[
        reference_date >= (max(as.Date(nowcast_date)) - max_delay)][,
        c("holiday") := NULL]

   plot <- epinowcast::enw_plot_nowcast_quantiles(
      summarised_nowcast,
      latest_obs = latest_obs[
        reference_date >= min(summarised_nowcast$reference_date)][
        reference_date <= max(summarised_nowcast$reference_date)
      ], ...
  )
  return(plot)
}