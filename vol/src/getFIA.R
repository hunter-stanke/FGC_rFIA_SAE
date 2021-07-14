##=====================================================
##=====================================================
##
## This script downloads all necessary FIA data for use
## in ... . All data were downloaded as csv files from 
## the public FIA Datamart on 1 December 2020 using
## the rFIA R package. 
##
## Last modified: 1 December 2020 - Hunter Stanke
##
##====================================================
##====================================================


## Load packages/ set working directory ----------------------------------------
library(rFIA)
library(here)



## Download/save data ----------------------------------------------------------
getFIA(states = c('ME'),
       dir = here('vol/data/FIA/'),
       load = FALSE)
