Supplementary information
================

# Setup

Set up the workflow pipeline and options. We first load the `targets`
package and remove the potentially outdated workflow.

``` r
library(targets)
library(tarchetypes)
library(data.table)
library(epinowcast)
library(ggplot2)
library(purrr)
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:data.table':
#> 
#>     transpose
library(here)
#> here() starts at /workspaces/eval-germany-sm-nowcasting
tar_unscript()
```

We now define shared global options across our workflow and load R
functions from the `R` folder.

``` r
library(targets)
library(tarchetypes)
library(data.table)
library(epinowcast)
library(ggplot2)
library(purrr)
library(here)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
tar_option_set(
  packages = c("data.table", "epinowcast", "scoringutils", "ggplot2", "purrr",
               "cmdstanr"),
  deployment = "worker",
  memory = "transient",
  workspace_on_error = TRUE,
  error = "continue",
  garbage_collection = TRUE
)
#> Establish _targets.R and _targets_r/globals/globals.R.
```

# Ingest data and stratify by location and report date

  - Specify todays date to trigger downloading new data.

<!-- end list -->

``` r
today <- Sys.Date()
#> Establish _targets.R and _targets_r/globals/today.R.
```

  - Download and process hospitalisation data.

<!-- end list -->

``` r
tar_target(hospitalisations, {
  get_germany_hospitalisations(today)
})
#> Define target hospitalisations from chunk code.
#> Establish _targets.R and _targets_r/targets/hospitalisations.R.
```

  - Download and process public holidays data.

  - Define dates to nowcast.

<!-- end list -->

``` r
tar_target(nowcast_dates, {
  unique(
    hospitalisations[reference_date >= as.Date("2021-09-01")]$reference_date
  )[1:2]
})
#> Define target nowcast_dates from chunk code.
#> Establish _targets.R and _targets_r/targets/nowcast_dates.R.
```

  - Define age groups

<!-- end list -->

``` r
tar_target(age_groups, {
  unique(hospitalisations$age_group)
})
#> Define target age_groups from chunk code.
#> Establish _targets.R and _targets_r/targets/age_groups.R.
```

  - Define locations to nowcast

<!-- end list -->

``` r
tar_target(locations, {
  unique(hospitalisations$location)[1:2]
})
#> Define target locations from chunk code.
#> Establish _targets.R and _targets_r/targets/locations.R.
```

  - Define maximum allowed reporting delay (in days).

<!-- end list -->

``` r
tar_target(max_report_delay, {
  40
})
#> Define target max_report_delay from chunk code.
#> Establish _targets.R and _targets_r/targets/max_report_delay.R.
```

  - Define hospitalisations by date of report

<!-- end list -->

``` r
tar_target(
  hospitalisations_by_date_report,
  enw_retrospective_data(
    hospitalisations, rep_date = nowcast_dates,
    ref_days = max_report_delay
  )[, nowcast_date := nowcast_dates],
  map(nowcast_dates),
  iteration = "list"
)
#> Establish _targets.R and _targets_r/targets/hospitalisations_by_date_report.R.
```

  - Define latest available data.

<!-- end list -->

``` r
tar_target(latest_hospitalisations, {
   enw_latest_data(hospitalisations)
})
#> Define target latest_hospitalisations from chunk code.
#> Establish _targets.R and _targets_r/targets/latest_hospitalisations.R.
```

  - Make latest observations into 7 day incidence.

<!-- end list -->

``` r
tar_target(latest_7day_hospitalisations, {
  copy(latest_hospitalisations)[,
    confirm := frollsum(confirm, n =  7), by = c("age_group", "location")
  ][!is.na(confirm)]
})
#> Define target latest_7day_hospitalisations from chunk code.
#> Establish _targets.R and _targets_r/targets/latest_7day_hospitalisations.R.
```

  - Plot reporting delay percentage by date, location, and age group.

# Fit models and produce nowcasts

## Model and model settings

  - Compile the model for multithread use.

<!-- end list -->

``` r
tar_target(
  epinowcast_model,
  compile_model(),
  format = "file", deployment = "main",
)
#> Establish _targets.R and _targets_r/targets/epinowcast_model.R.
```

  - Define stan settings shared across models and used for fitting

<!-- end list -->

``` r
tar_target(epinowcast_settings, {
  list(
    save_warmup = FALSE,
    output_loglik = FALSE,
    pp = FALSE,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 2,
    parallel_chains = 2,
    threads_per_chain = 2,
    adapt_delta = 0.8,
    show_messages = FALSE,
    refresh = 0
  )
})
#> Define target epinowcast_settings from chunk code.
#> Establish _targets.R and _targets_r/targets/epinowcast_settings.R.
```

## Priors

  - Define a set of uninformed priors using the defaults from
    `epinowcast`.

<!-- end list -->

``` r
tar_target(uninformed_priors, {
  enw_priors()
})
#> Define target uninformed_priors from chunk code.
#> Establish _targets.R and _targets_r/targets/uninformed_priors.R.
```

  - Define a set of observations to use as a source for informed priors.
    Here we use overall national hospitalisations and data from the 1st
    of September 2021.

<!-- end list -->

``` r
tar_target(prior_obs, {
  enw_retrospective_data(
    hospitalisations[location == "DE"][age_group == "00+"],
    rep_date = as.Date("2021-09-01"), ref_days = max_report_delay
  )
})
#> Define target prior_obs from chunk code.
#> Establish _targets.R and _targets_r/targets/prior_obs.R.
```

  - Fit a model to national overall hospitalisations only and extract
    posterior estimates for the delay distribution and overdispersion to
    use as informed priors for other models by assuming a normal
    distribution with their posterior standard deviations scaled by 5
    times.

<!-- end list -->

``` r
tar_target(priors, {
  do.call(prior_epinowcast, c(
    list(
      prior_obs, max_delay = max_report_delay, scale = 5,
      priors = uninformed_priors
    ),
    epinowcast_settings
  ))
})
#> Define target priors from chunk code.
#> Establish _targets.R and _targets_r/targets/priors.R.
```

## Models

  - Intercept only model.

<!-- end list -->

``` r
tar_target(
  fixed_nowcast,
  nowcast(
    obs = hospitalisations_by_date_report, 
    tar_loc = locations,
    model = fixed_epinowcast,
    priors = priors,
    max_delay = max_report_delay,
    settings = epinowcast_settings
  ),
  cross(hospitalisations_by_date_report, locations)
)
#> Establish _targets.R and _targets_r/targets/fixed_nowcast.R.
```

  - Intercept model with day of the week reporting effects.

<!-- end list -->

``` r
tar_target(
  dow_nowcast,
  nowcast(
    obs = hospitalisations_by_date_report,
    tar_loc = locations,
    model = dow_epinowcast,
    priors = priors,
    max_delay = max_report_delay,
    settings = epinowcast_settings
  ),
  cross(hospitalisations_by_date_report, locations)
)
```

  - Age group random effect model with day of the week effect reporting
    effects.

<!-- end list -->

``` r
tar_target(
  age_nowcast,
  nowcast(
    obs = hospitalisations_by_date_report,
    tar_loc = locations,
    model = age_epinowcast,
     priors = priors,
    max_delay = max_report_delay,
     settings = epinowcast_settings
  ),
  cross(hospitalisations_by_date_report, locations)
)
```

  - Age group random effect model with a weekly random walk and a day of
    the week reporting effect.

<!-- end list -->

``` r
tar_target(
  week_nowcast,
  nowcast(
    obs = hospitalisations_by_date_report,
    tar_loc = locations,
    model = week_epinowcast,
    priors = priors,
    max_delay = max_report_delay,
    settings = epinowcast_settings
  ),
  cross(hospitalisations_by_date_report, locations)
)
```

  - Age group random effect model with a weekly random walk, an age
    group specific weekly random walk in the residuals, and a day of the
    week reporting effect.

<!-- end list -->

``` r
tar_target(
  age_week_nowcast,
  nowcast(
    obs = hospitalisations_by_date_report,
    tar_loc = locations,
    model = age_week_epinowcast,
    priors = priors,
    max_delay = max_report_delay,
    settings = epinowcast_settings
  ),
  cross(hospitalisations_by_date_report, locations)
)
```

  - Independent age group model with a weekly random walk and a day of
    the week reporting model.

<!-- end list -->

``` r
tar_target(
  independent_nowcast,
  nowcast(
    obs = hospitalisations_by_date_report[age_group == age_groups],
    tar_loc = locations,
    model = independent_epinowcast,
    priors = priors,
    max_delay = max_report_delay,
    settings = epinowcast_settings
  ),
  cross(hospitalisations_by_date_report, locations, age_groups)
)
```

# Postprocess

  - Save nowcasts stratified by nowcasting date.

  - Format 7 day nowcasts for hub submission.

  - Save 7 day nowcasts formatted for hub submission.

  - Save latest hospitalisation data

  - Save 7 day hospitalisation data

# Visualise

  - Plot most recent nowcast by location, age group, and model.

  - Plot nowcasts at 0 horizon by location, age group, and model.

  - Plot nowcasts at 3 day horizon by location, age group and model

# Evaluation

  - Score nowcasts overall both on a natural scale and on a log scale.

  - Score nowcasts by location on a natural scale and on a log scale.

  - Score nowcasts by age group on a natural scale and on a log scale.

  - Score nowcasts by horizon on a natural scale and on a log scale.

  - Plot scores relative to the baseline by location and age group on
    both the natural and log scale.

# Reporting

  - Render nowcast report.

# Pipeline

The pipeline can be regenerated by rendering this file,

``` r
rmarkdown::render("_targets.Rmd")
```

The pipeline can then be run using,

``` r
tar_make()
```

The complete pipeline can be visualised using,

``` r
tar_visnetwork()
```

The pipeline can be regenerated and run using the following single step

``` bash
. _targets.sh
```

Alternatively the pipeline can be explored interactively using this
notebook.
