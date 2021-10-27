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

#' Extract a Parameter Summary from a Stan Object
#'
#' @description `r lifecycle::badge("stable")`
#' Extracts summarised parameter posteriors from a `stanfit` object using
#' `rstan::summary` in a format consistent with other summary functions in
#' `EpiNow2`.
#' @param fit A `stanfit` object
#' @param params A character vector of parameters to extract. Defaults to all parameters.
#' @param var_names Logical defaults to `FALSE`. Should variables be named. Automatically set
#' to TRUE if multiple parameters are to be extracted.
#' @return A `data.table` summarising parameter posteriors. Contains a following variables:
#' `variable`, `mean`, `mean_se`, `sd`, `median`, and `lower_`, `upper_` followed by
#' credible interval labels indicating the credible intervals present.
#' @inheritParams calc_summary_measures
#' @export
#' @importFrom data.table as.data.table :=
#' @importFrom rstan summary
extract_stan_param <- function(fit, params = NULL,
                               CrIs = c(0.2, 0.5, 0.9), var_names = FALSE) {
  # generate symmetric CrIs
  CrIs <- CrIs[order(CrIs)]
  sym_CrIs <- c(0.5, 0.5 - CrIs / 2, 0.5 + CrIs / 2)
  sym_CrIs <- sym_CrIs[order(sym_CrIs)]
  CrIs <- round(100 * CrIs, 0)
  CrIs <- c(paste0("lower_", rev(CrIs)), "median", paste0("upper_", CrIs))
  args <- list(object = fit, probs = sym_CrIs)
  if (!is.null(params)) {
    if (length(params) > 1) {
      var_names <- TRUE
    }
    args <- c(args, pars = params)
  } else {
    var_names <- TRUE
  }
  summary <- do.call(rstan::summary, args)
  summary <- data.table::as.data.table(summary$summary,
    keep.rownames = ifelse(var_names,
      "variable",
      FALSE
    )
  )
  cols <- c("mean", "se_mean", "sd", CrIs, "n_eff", "Rhat")
  if (var_names) {
    cols <- c("variable", cols)
  }
  colnames(summary) <- cols
  summary <- summary[, c("n_eff", "Rhat") := NULL]
  return(summary)
}

inv_logit <- function(x) {
  il <- 1 / (1 + exp(-x))
  return(il)
}

logit <- function(p) {
  l <- log(p / (1 - p))
  return(l)
}
