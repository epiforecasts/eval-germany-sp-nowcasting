#' Save a plot and return the path for targets
#' @importFrom ggplot2 ggsave
save_plot <- function(plot, filename, path, ...) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(path, file)
  ggplot2::ggsave(path, plot, ...)
  return(filename)
}

#' Save a dataframe to a csv and return the path for targets
save_csv <- function(dt, filename, path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(path, filename)
  data.table::fwrite(dt, path)
  return(path)
}

drop_string <- function(var, string) {
  var[!grepl(string, var)]
}

select_var <- function(dt, var) {
  dt[[var]]
}

load_nowcasts <- function(path) {
  nowcasts <- fs::dir_ls(
    path,
    glob = "*.csv"
  ) |>
    purrr::map(data.table::fread) |>
    data.table::rbindlist()
  nowcasts[, horizon := as.numeric(
    as.Date(reference_date) - as.Date(nowcast_date)
  )]
  if (!is.null(nowcasts$age_group)) {
    nowcasts[
    ,
      age_group := factor(
        age_group,
        levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+")
      )
  ]
  }
  nowcasts[,
   model := factor(
    model,
    levels = c("Reference: Fixed, Report: Fixed",
               "Reference: Fixed, Report: Day of week",
               "Reference: Age, Report: Day of week",
               "Reference: Age and week, Report: Day of week",
               "Reference: Age and week by age, Report: Day of week",
               "Independent by age, Reference: Week, Report: Day of week")
   )
  ]
  return(nowcasts[])
}

load_obs <- function(path) {
  obs <- fread(path)
  obs[
    ,
    age_group := factor(
      age_group,
      levels = c("00+", "00-04", "05-14", "15-34", "35-59", "60-79", "80+")
    )
  ]
  return(obs[])
}

fancy_datatable <- function(dt) {
  dt <- janitor::clean_names(dt, case = "sentence")
  DT::datatable(
    dt,
    extensions = c("Buttons", "Responsive"), options = list(
      dom = "Bfrtip", buttons = c("csv")
    )
  )
}