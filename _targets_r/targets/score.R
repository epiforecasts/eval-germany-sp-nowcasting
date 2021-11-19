tar_map(
  list(score_by = c("overall", "location", "age_group", "horizon")),
  tar_target(
    scores,
    enw_score_nowcast(
      scored_nowcasts, complete_hospitalisations, 
      summarise_by = ifelse(score_by %in% "overall", "model", 
                            c(score_by, "model")),
      log = FALSE
    )
  ),
  tar_target(
    log_scores,
    enw_score_nowcast(
      scored_nowcasts, complete_hospitalisations, 
      summarise_by = ifelse(score_by %in% "overall", "model", 
                            c(score_by, "model")),
      log = TRUE
    )
  ),
  tar_target(
    save_scores,
    save_csv(
      rind(scores[, scale := "natural"], log_scores[, scale := "log"]),
      filename = paste0(score_by, ".csv"),
      path = here("data/scores")
    ),
    format = "file"
  )
)
