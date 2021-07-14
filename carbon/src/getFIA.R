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
# Vector of states
allStates <- c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'ID',
               'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS',
               'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK',
               'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV',
               'WI', 'WY')

# Download
getFIA(states = allStates,
       dir = here('carbon/data/FIA/'),
       load = FALSE)
