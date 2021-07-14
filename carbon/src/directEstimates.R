

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
db <- readFIA(dir = '/home/hunter/FIA', #dir = here('carbon/data/FIA'),
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
  
