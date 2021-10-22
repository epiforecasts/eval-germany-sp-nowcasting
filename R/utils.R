#' Save a plot and return the ppath for targets
#' @importFrom ggplot2 ggsave
save_plot <- function(plot, filename, ...) {
  ggplot2::ggsave(filename, plot, ...)
  return(filename)
}

#' Save a dataframe to a csv and return the path for targets
save_csv <- function(dt, path) {
  data.table::fwrite(dt, path)
  return(path)
}

#' Expose stan functions
#'
#' @importFrom rstan expose_stan_functions
#' @importFrom purrr map_chr
expose_stan_fns <- function(files, target_dir, ...) {
  functions <- paste0(
    "\n functions{ \n",
    paste(purrr::map_chr(
      files,
      ~ paste(readLines(file.path(target_dir, .)), collapse = "\n")
    ),
    collapse = "\n"
    ),
    "\n }"
  )
  rstan::expose_stan_functions(rstan::stanc(model_code = functions), ...)
  return(invisible(NULL))
}
