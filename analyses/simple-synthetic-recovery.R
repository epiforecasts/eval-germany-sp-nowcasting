library(covidregionaldata)
library(data.table)
library(rstan)
library(here)
library(purrr)
source(here("R", "nowcast.R"))
source(here("R", "simulate.R"))
# set number of cores to use
options(mc.cores = 4)
# get example case counts
start_using_memoise()
reported_cases <- get_national_data("UK", verbose = FALSE)
reported_cases <- setDT(reported_cases)[date >= as.Date("2021-07-01")]
reported_cases <- reported_cases[, .(date, confirm = cases_new)]

# define example truncation distribution (note not integer adjusted)
trunc_dist <- list(
  mean = 1.8,
  mean_sd = 0.01,
  sd = 0.6,
  sd_sd = 0.01,
  max = 20
)

# apply truncation to example data
example_data <- map(c(40, 30, 25, 20, 15, 10, 5, 0),
  simulate_simple_truncation,
  cases = reported_cases,
  dist = trunc_dist
)
example_data <- map(example_data, ~ .[, report_date := max(date)])
example_data <- rbindlist(example_data)

# load model
model <- stan_model(here("stan", "nowcast.stan"))

# fit model to example data and produce nowcast
est <- nowcast(example_data, model = model, max_truncation = 20)

# summary of the distribution
est$dist
# summary of the estimated truncation cmf (can be applied to new data)
print(est$cmf)
# observations linked to truncation adjusted estimates
print(est$nowcast)

# Plot nowcast vs actually observed
plot(est, latest_obs = reported_cases)

# Plot retrospective nowcast for previous observations
plot(est, latest_obs = reported_cases, type = "re")

# Plot posterior prediction for observed cases at date of report
plot(est, latest_obs = reported_cases, type = "pos", log = TRUE)
