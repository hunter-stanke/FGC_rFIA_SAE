##=====================================================
##=====================================================
##
## This script computes direct estimates of county-level
## forest carbon stocks using FIA's traditional, 
## post-stratified estimators. Note that we use a spatial
## dataset to delineate counties, as opposed to 
## specifying `grpBy = COUNTYCD` in the call to `carbon`.
## Both approaches will yield the same results, but we
## take the spatial approach here to improve the 
## generality of this code for adaptation by subsequent
## users.
##
## For reference, this script will load FIA data 
## previously downloaded by running the script `getFIA.R`.
## Hence, `getFIA.R` must be run first. Results will be
## saved in the `carbon/results/` directory, labeled as
## `direct_estimates.csv`. These direct estimates will 
## then be used to fit a Fay-Herriot model (implemented in
## `fitModel.R`). Note that `getPRISM.R` must also be 
## run before `fitModel.R`.
##
## Last modified: 22 July 2021 - Hunter Stanke
##
##====================================================
##====================================================

## Load packages/ set working directory ----------------------------------------
library(rFIA)
library(here)
library(sf)


# Number of physical cores to use 
# Check what you have w/ parallel::detectCores(logical=FALSE)
cores = 10




## Set up "remote" FIA Database ------------------------------------------------
# Vector of states
allStates <- c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'ID',
               'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS',
               'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK',
               'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV',
               'WI', 'WY')

# Set up database
db <- readFIA(dir = here('carbon/data/FIA'),
              states = allStates,
              inMemory = FALSE,
              nCores = cores)

# A most recent subset
db <- clipFIA(db)




## Read spatial data -----------------------------------------------------------
counties <- st_read(here('carbon/data/GIS/counties/')) 




## Estimate carbon density by county -------------------------------------------
pop.est <- carbon(db, 
                  polys = counties,
                  totals = TRUE,
                  variance = TRUE,
                  byPool = FALSE, # Sum up all pools, total forest carbon
                  nCores = cores)
  
  
  

## Save results ----------------------------------------------------------------
write.csv(pop.est, 
          here('carbon/results/direct_estimates.csv'), 
          row.names = FALSE)
  
