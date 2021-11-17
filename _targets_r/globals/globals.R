library(targets)
library(tarchetypes)
library(data.table)
library(epinowcast)
library(ggplot2)
library(purrr, quietly = TRUE)
library(here)
library(future)
library(future.callr)
plan(callr)
functions <- list.files(here("R"), full.names = TRUE)
walk(functions, source)
rm("functions")
tar_option_set(
  packages = c("data.table", "epinowcast", "scoringutils", "ggplot2", "purrr",
               "cmdstanr"),
  deployment = "worker",
  memory = "transient",
  workspace_on_error = TRUE,
  error = "continue",
  garbage_collection = TRUE
)
