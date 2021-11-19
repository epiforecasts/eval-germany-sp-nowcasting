tar_target(complete_7day_hospitalisations, {
  latest_7day_hospitalisations[
    reference_date < (max(reference_date) - 28)][,
    horizon := as.numeric(reference_date - max(reference_date))
  ]
})
