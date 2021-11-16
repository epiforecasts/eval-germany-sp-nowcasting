get_germany_hospitalisations <- function(url = "https://raw.githubusercontent.com/jbracher/hospitalization-nowcast-hub/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv") { # nolint
  germany_hosp <- data.table::fread(url)

  germany_hosp <- data.table::melt(
    germany_hosp,
    variable.name = "delay",
    value.name = "confirm",
    id.vars = c("date", "location", "age_group")
  )
  data.table::setnames(germany_hosp, "date", "reference_date")

  germany_hosp[, report_date := as.Date(reference_date) + 0:(.N - 1),
    by = c("reference_date", "location", "age_group")
  ]
  germany_hosp <- germany_hosp[report_date <= max(reference_date)]
  germany_hosp[, delay := NULL]
  germany_hosp[is.na(confirm), confirm := 0]
  germany_hosp[, confirm := cumsum(confirm),
    by = c("reference_date", "location", "age_group")
  ]
  return(germany_hosp[])
}
