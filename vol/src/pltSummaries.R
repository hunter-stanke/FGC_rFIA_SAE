##=====================================================
##=====================================================
##
## This script (1) computes plot-level summaries of 
## merchantable wood volume for all plot visits recorded
## by FIA in Maine, and (2) extracts survey design 
## information related to the most recent "current
## volume" inventory in the state (e.g., strata weights,
## plot assignments to stratum, etc).
##
## For reference, this script will load FIA data 
## previously downloaded by running the script `getFIA.R`.
## Hence, `getFIA.R` must be run first. Results will be
## saved in the `volume/results/` directory, labeled as
## `plt_summaries.csv`. These plot-level estimates will 
## then be used to fit the Bayesian mixed-effects model
## (implemented in `fitModel.R`). 


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



## Prep FIA Data for subsequent modeling ---------------------------------------

## Extract stratum and estimation units weights for the most recent 
## current volume inventory in each state
wgts <- getDesignInfo(db, type = 'VOL', mostRecent = TRUE)

## Summarize merchantable volume to the plot level for all plots in the
## Washington survey unit
bio <- volume(db,
              byPlot = TRUE,
              areaDomain = UNITCD == 1,
              landType = 'timber',
              nCores = 10)

## Join design info and plot-level time-series
dat <- wgts %>%
  ## Drop inventory year, YEAR will indicate plot measurement year now
  ## PLT_CN only helps us identify the plot visits that are included in 
  ## the most recent volume inventory, we want a time-series instead, 
  ## so we use the permanent plot identifier `pltID`
  select(-c(YEAR, PLT_CN)) %>%
  left_join(select(bio, pltID, PLT_CN, YEAR, BOLE_CF_ACRE), by = c('pltID')) %>%
  ## We're safe to drop estimation units where no non-zero values of the 
  ## response variables are ever observed
  group_by(ESTN_UNIT_CN) %>%
  mutate(nonzero = sum(BOLE_CF_ACRE, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(nonzero > 0) %>%
  select(STATECD, ESTN_UNIT_CN, AREA_USED, STRATUM_CN, 
         STRATUM_WGT, pltID, YEAR, BOLE_CF_ACRE)


## Save results ----------------------------------------------------------------
write.csv(dat, here('vol/results/plt_summaries.csv'), row.names = FALSE)

