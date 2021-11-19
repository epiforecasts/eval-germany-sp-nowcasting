tar_target(complete_7day_hospitalisations, {
  latest_7day_hospitalisations[reference_date < (report_date - 28)][,
                               horizon := as.numeric(reference_date - report_date)
                              ]
})
