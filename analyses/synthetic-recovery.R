library(covidregionaldata)
suppressMessages(library(data.table, quietly = TRUE))
suppressMessages(library(rstan, quietly = TRUE))
library(here)
suppressMessages(library(purrr))
source(here("R", "epinowcast.R"))
source(here("R", "models.R"))
source(here("R", "simulate.R"))
source(here("R", "scenarios.R"))

# set number of cores to use
options(mc.cores = 4)
# get example case counts
start_using_memoise()
latest_cases <- get_national_data("UK", verbose = FALSE)
latest_cases <- setDT(latest_cases)[date >= as.Date("2021-07-01")]
latest_cases <- latest_cases[, .(date, confirm = cases_new)]

# get a range of dates to generate synthetic data for
scenarios <- enw_random_intercept_scenario(
  obs = latest_cases,
  snapshots = c(30:0),
  logmean = 1.9, logmean_sd = 0.1,
  logsd = 1, logsd_sd = 0.1
)

# simulate observations
scenarios <- enw_simulate_lnorm_trunc_obs(scenarios, latest_cases)
sim_reported_cases <- rbindlist(scenarios$reported_cases)

# Construct design matrices for the desired effects
effects <- enw_intercept_model(sim_reported_cases)

# compile model
model <- rstan::stan_model(here("stan", "nowcast.stan"))

# fit model to example data and produce nowcast
est <- epinowcast(sim_reported_cases,
  model = model,
  design = effects$design, design_sd = effects$design_sd,
  control = list(max_treedepth = 12, adapt_delta = 0.8),
  max_truncation = 20
)

# observations linked to truncation adjusted estimates
est$nowcast[[1]]

# Plot nowcast vs latest observations
plot(est, obs = latest_cases)

# Plot posterior prediction for observed cases at date of report
plot(est, obs = latest_cases, type = "pos", log = TRUE)
