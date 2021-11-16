Supplementary information
================

# Setup

Set up the workflow pipeline and options. We first load the `targets`
package and remove the potentially outdated workflow.

``` r
library(targets)
library(tarchetypes)
library(tibble)
library(tidyr)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(scoringutils)
#> Note: The definition of the weighted interval score has slightly changed in version 0.1.5. If you want to use the old definition, use the argument `count_median_twice = TRUE` in the function `eval_forecasts()`
library(ggplot2)
library(purrr)
#> 
#> Attaching package: 'purrr'
#> The following object is masked from 'package:scoringutils':
#> 
#>     update_list
library(here)
#> here() starts at /workspaces/eval-germany-sm-nowcasting
tar_unscript()
```

We now define shared global options across our workflow and load R
functions from the `R` folder.

``` r
library(purrr)
library(here)
library(tarchetypes)
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

  - Download and process hospitalisation data.

<!-- end list -->

``` r
tar_target(hospitalisations, {
  get_germany_hospitalisations()
})
#> Define target hospitalisations from chunk code.
#> Establish _targets.R and _targets_r/targets/hospitalisations.R.
```

  - Download and process public holidays data.

  - Define dates to nowcast.

<!-- end list -->

``` r
tar_target(nowcast_dates, {
  hospitalisations[reference_date >= as.Date("2021-09-01")]$reference_date[1:7]
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
  unique(hospitalisations$location)
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
  map(nowcast_dates)
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

  - Plot reporting delay percentage by date, location, and age group.

# Fit models and produce nowcast

## Model and model settings

  - Compile the model for multithread use.

<!-- end list -->

``` r
tar_target(
  epinowcast_model,
  compile_model(threads = TRUE),
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
    iter_sampling = 2000,
    chains = 2,
    threads_per_chain = 2,
    adapt_delta = 0.95,
    show_messages = FALSE
  )
})
#> Define target epinowcast_settings from chunk code.
#> Establish _targets.R and _targets_r/targets/epinowcast_settings.R.
```

## Models

  - Intercept only model.

  - Intercept model with day of the week reporting effects.

  - Age group random effect model with day of the week effect reporting
    effects.

  - Age group random effect model with a weekly random walk and a day of
    the week reporting effect.

  - Age group random effect model with a weekly random walk, an age
    group specific weekly random walk in the residuals, and a day of the
    week reporting effect.

  - Independent age group model with a weekly random walk and a day of
    the week reporting model.

# Postprocess

  - Make nowcasts into 7 day incidence.

  - Make latest observations into 7 day incidence.

  - Save nowcasts stratified by nowcasting date.

  - Format nowcasts for hub submission.

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
