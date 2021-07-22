##=====================================================
##=====================================================
##
## This script computes direct estimates of annual
## merchantable wood volume in Washington County Maine,
## using FIA's traditional, post-stratified estimators. 
##
## For reference, this script will load FIA data 
## previously downloaded by running the script `getFIA.R`.
## Hence, `getFIA.R` must be run first. Results will be
## saved in the `volume/results/` directory, labeled as
## `annual_panels.csv`. These direct estimates will 
## then be used to fit a Bayesian mixed-effects model
## (implemented in `fitModel.R`). Note that
## `pltSummaries.R` must also be run before `fitModel.R`.
##
## Last modified: 22 July 2021 - Hunter Stanke
##
##====================================================
##====================================================


## Load packages/ set working directory ----------------------------------------
library(rFIA)
library(dplyr)
library(here)


## Set up a remote database ----------------------------------------------------
db <- readFIA(dir = here('vol/data/FIA'), 
              states = c('ME'),
              inMemory = FALSE)



## Annual estimator of total merchantable volume -------------------------------
## Washington survey unit, ME
bio <- volume(db,
               landType = 'timber',
               areaDomain = UNITCD == 1,
               method = 'annual',
               totals = TRUE,
               variance = TRUE,
               nCores = 10)

## Save results ----------------------------------------------------------------
write.csv(bio, here('vol/results/annual_panels.csv'), row.names = FALSE)

