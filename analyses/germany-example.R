suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(rstan, quietly = TRUE))
library(purrr)
library(here)

# load dev code
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)

# set number of cores to use
options(mc.cores = 4)

# get processed germany data and reprocess
germany_hosp <- fread(
  "https://raw.githubusercontent.com/jbracher/hospitalization-nowcast-hub/main/data-truth/COVID-19/COVID-19_hospitalizations_preprocessed.csv" # nolint
)
germany_hosp <- melt(
  germany_hosp,
  variable.name = "delay",
  value.name = "confirm",
  id.vars = c("date", "location", "age_group")
)
setnames(germany_hosp, "date", "reference_date")

germany_hosp[, report_date := as.Date(reference_date) + 0:(.N - 1),
  by = c("reference_date", "location", "age_group")
]
germany_hosp <- germany_hosp[report_date <= max(reference_date)]
germany_hosp[, delay := NULL]
germany_hosp[is.na(confirm), confirm := 0]

# national only and only for the last 28 days of data
germany_nat_hosp <- germany_hosp[location == "DE"]
germany_nat_hosp <-
  germany_nat_hosp[reference_date >= (max(reference_date) - 27)]

# Preprocess data
pobs <- enw_preprocess_data(germany_nat_hosp, by = "age_group", max_delay = 20)

# Construct design matrices for the desired reference date effects
reference_effects <- enw_intercept_model(pobs$metareference[[1]])

# Construct design matrices for the desired report day effects
report_effects <- enw_day_of_week_model(pobs$metareport[[1]])

# compile model
model <- rstan::stan_model(here("stan", "nowcast.stan"))

# fit model to example data and produce a nowcast
est <- epinowcast(pobs,
  model = model,
  report_effects = report_effects, reference_effects = reference_effects,
  control = list(max_treedepth = 10, adapt_delta = 0.8),
  debug = FALSE, pp = FALSE, save_warmup = FALSE,
)

# observations linked to truncation adjusted estimates
est$nowcast[[1]]

# Plot nowcast vs latest observations
plot(est, obs = latest_cases)

# Plot posterior prediction for observed cases at date of report
plot(est, obs = latest_cases, type = "pos", log = TRUE)
