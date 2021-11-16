tar_target(nowcast_dates, {
  hospitalisations[reference_date >= as.Date("2021-09-01")]$reference_date[1:7]
})
