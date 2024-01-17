#!/bin/bash
Rscript -e "fpath <- file.path('inst', 'script', 'create_metadata_tables.R'); \
source(fpath); \
writeLines(date(), file.path('inst', 'extdata', 'date_of_last_update.txt')); \
roxygen2::roxygenize()"
