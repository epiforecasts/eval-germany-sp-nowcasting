tar_target(complete_hospitalisations, {
  latest_hospitalisations[reference_date < (report_date - 28)][,
                          horizon := as.numeric(reference_date - report_date)]
})
