library(covidregionaldata)
library(data.table)
library(rstan)
library(here)
source(here("R", "estimate_truncation.R"))
# set number of cores to use
options(mc.cores = 4)
# get example case counts
reported_cases <- get_national_data("UK")
reported_cases <- setDT(reported_cases)[date >= as.Date("2021-07-01")]
reported_cases <- reported_cases[, .(date, confirm = cases_new)]

# load model
model <- rstan::stan_model(here("stan", "estimate_truncation.stan"))

# define example truncation distribution (note not integer adjusted)
trunc_dist <- list(
  mean = 2.1,
  mean_sd = 0.1,
  sd = 0.4,
  sd_sd = 0.01,
  max = 20
)

# apply truncation to example data
construct_truncation <- function(index, cases, dist) {
  set.seed(index)
  cmf <- cumsum(
    dlnorm(
      1:(dist$max + 1),
      rnorm(1, dist$mean, dist$mean_sd),
      rnorm(1, dist$sd, dist$sd_sd)
    )
  )
  cmf <- cmf / cmf[dist$max + 1]
  cmf <- rev(cmf)[-1]
  trunc_cases <- data.table::copy(cases)[1:(.N - index)]
  trunc_cases[
    (.N - length(cmf) + 1):.N,
    confirm := purrr::map(confirm * cmf, ~ rpois(1, .))
  ]
  return(trunc_cases)
}
example_data <- purrr::map(c(30, 25, 20, 15, 10, 0),
  construct_truncation,
  cases = reported_cases,
  dist = trunc_dist
)

# fit model to example data
est <- estimate_truncation(example_data, model = model, max_truncation = 20)

# summary of the distribution
est$dist
# summary of the estimated truncation cmf (can be applied to new data)
print(est$cmf)
# observations linked to truncation adjusted estimates
print(est$obs)
# validation plot of observations vs estimates
plot(est)
