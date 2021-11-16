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
