summarised_nowcast[reference_date >= (report_date - 7)][
                   reference_date < (report_date - 28)][,
                   holiday := NULL][,
                   horizon := as.numeric(reference_date - report_date)]
