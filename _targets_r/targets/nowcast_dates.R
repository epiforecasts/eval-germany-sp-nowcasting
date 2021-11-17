tar_target(nowcast_dates, {
  unique(
    hospitalisations[reference_date >= as.Date("2021-09-01")]$reference_date
  )[1:2]
})
