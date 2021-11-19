unnest_nowcasts <- function(nowcasts, target) {
  nowcasts <- nowcasts[,
    (target) := pmap(
      list(get(target), model, nowcast_date),
        function(df, m, d) {
          df[, model := m][, nowcast_date := d]
        })][,
    rbindlist(get(target), use.names = TRUE)][
    order(location, age_group, nowcast_date, reference_date, model)
    ]

  data.table::setcolorder(nowcasts, neworder = c("model", "nowcast_date"))
  nowcasts[, nowcast_date := as.Date(nowcast_date)]
  nowcasts[, report_date := nowcast_date]
  return(nowcasts[])
}

format_for_submission <- function(nowcast, horizon = -28,
                                  pathogen = "COVID-19") {
  long <- epinowcast::enw_quantiles_to_long(nowcast)
  long <- long[
    as.character(quantile) %in% as.character(
      c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
    )
  ]
  long[quantile == "0.5", quantile := NA]
  long[, days_since_nowcast := as.numeric(
    as.Date(reference_date) - as.Date(nowcast_date)
  )]
  long <- long[days_since_nowcast >= horizon]
  long[, `:=`(
    target = paste0(
      days_since_nowcast, " day ahead inc hosp"
    ),
    type = "quantile"
  )]
  long[is.na(quantile), type := "mean"]
  long <- long[, .(location, age_group,
    forecast_date = nowcast_date,
    target_end_date = reference_date, target, type, quantile,
    value = prediction, pathogen = pathogen
  )]
  long[order(location, age_group, forecast_date, target_end_date)]
  return(long)
}