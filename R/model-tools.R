compile_model <- function(...) {
  model <- epinowcast::enw_model(thread = TRUE, ...)
  return(model$exe_file())
}

load_model <- function(model_file) {
  model <- suppressMessages(
    cmdstanr::cmdstan_model(
      exe_file = model_file,
      cpp_options = list(stan_threads = TRUE)
    )
  )
  return(model)
}

summarise_nowcast <- function(nowcast, model,
                              probs = c(
                                0.025, 0.05, seq(0.1, 0.9, by = 0.1),
                                0.95, 0.975
                              )) {
  daily <- summary(nowcast, type = "nowcast", probs = probs)

  samples <- summary(nowcast, type = "nowcast_samples")

  cols <- c("confirm", "sample")
  samples[, (cols) := lapply(.SD, data.table::frollsum, n = 7),
    .SDcols = cols, by = ".draw"
  ][!is.na(sample)]

  # Summarise 7 day nowcast
  seven_day <- enw_summarise_samples(samples, probs = probs)

  out <- data.table::data.table(
    model = model,
    nowcast_date = max(daily$reference_date),
    daily = list(daily),
    seven_day = list(seven_day)
  )
  return(out[])
}

nowcast <- function(obs, tar_loc, tar_date, model, model_file,
                    max_delay, settings) {
  do.call(
    model, c(
      list(
        obs = obs[location == tar_loc][nowcast_date == tar_date],
        max_delay = max_delay,
        model_file = model_file
      ),
      settings
    )
  )
}
