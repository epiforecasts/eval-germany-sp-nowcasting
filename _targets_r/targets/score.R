tar_map(
  list(score_by = c("overall", "age_group", "horizon")),
  tar_target(
    scores,
    enw_score_nowcast(
      scored_nowcasts, complete_hospitalisations, 
      summarise_by = drop_string(c(score_by, "model"), "overall"),
      log = FALSE
    )
  ),
  tar_target(
    log_scores,
    enw_score_nowcast(
      scored_nowcasts, complete_hospitalisations, 
      summarise_by = drop_string(c(score_by, "model"), "overall"),
      log = TRUE
    )
  ),
  tar_target(
    save_scores,
    save_csv(
      rbind(scores[, scale := "natural"], log_scores[, scale := "log"]),
      filename = paste0(score_by, ".csv"),
      path = here("data/scores")
    ),
    format = "file"
  )
)
