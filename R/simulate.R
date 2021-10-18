simulate_simple_truncation <- function(index, cases, dist) {
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
    confirm := purrr::map_dbl(confirm * cmf, ~ rpois(1, .))
  ]
  return(trunc_cases)
}
