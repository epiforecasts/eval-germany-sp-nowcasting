#!bin/bash

# render the workflow
Rscript -e "rmarkdown::render('_targets.Rmd')" 

nohup \
  Rscript -e "targets::tar_make_future(workers = future::availableCores())" \
  > targets.log 2>&1 &
