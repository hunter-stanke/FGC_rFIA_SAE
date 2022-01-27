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
library(tidyr)
library(here)


## Set up a remote database ----------------------------------------------------
db <- rFIA::readFIA(dir = here::here('vol/data/FIA'))



## Prep FIA Data for subsequent modeling ---------------------------------------

## Extract stratum and estimation units weights for the most recent 
## current volume inventory in each state
wgts <- rFIA::getDesignInfo(db, type = 'VOL', mostRecent = TRUE)

## Generate list of all visits to plots included in the most recent
## current volume inventory, including non-forested plots
plt.visits <- db$PLOT %>%
  # Replicate the unique plot ID (`pltID`) from rFIA
  dplyr::mutate(pltID = paste(UNITCD, STATECD, COUNTYCD, PLOT, sep = '_')) %>%
  # Select plots from inventory of interest
  dplyr::filter(pltID %in% wgts$pltID) %>%
  # Drop any visits pre-1999
  dplyr::filter(MEASYEAR >= 1999) %>%
  # Only retain ID columns
  dplyr::select(pltID, YEAR = MEASYEAR)

mod.dat <- wgts %>%
  ## Drop YEAR from design info, as it represents reporting years, 
  # as opposed to measurement years of plots
  dplyr::select(-YEAR) %>%
  dplyr::left_join(plt.visits, by = 'pltID')

## Summarize merchantable volume to the plot level for all plots in the
## Washington survey unit
plt.vol <- rFIA::volume(db,
                        byPlot = TRUE,
                        areaDomain = UNITCD == 1,
                        landType = 'timber')


mod.dat <- mod.dat %>%
  ## Join response variable onto plot-visit dataset
  dplyr::left_join(dplyr::select(plt.vol, pltID, PLT_CN, BOLE_CF_ACRE), 
                   by = c('pltID', 'PLT_CN')) %>%
  # Replace any NA's (indicating non-forested plots) with 0s
  tidyr::replace_na(list(BOLE_CF_ACRE = 0)) %>%
  # We're safe to drop estimation units where no non-zero values of the 
  # response variables are ever observed
  dplyr::group_by(ESTN_UNIT_CN) %>%
  dplyr::mutate(nonzero = sum(BOLE_CF_ACRE, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(nonzero > 0) %>%
  dplyr::select(STATECD, ESTN_UNIT_CN, AREA_USED, STRATUM_CN, 
                STRATUM_WGT, pltID, YEAR, BOLE_CF_ACRE)


## Save results ----------------------------------------------------------------
write.csv(mod.dat, 
          here::here('vol/results/plt_summaries.csv'),
          row.names = FALSE)

