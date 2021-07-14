
## Load packages/ set working directory ----------------------------------------
library(rFIA)
library(dplyr)
library(here)


## Set up a remote database ----------------------------------------------------
db <- readFIA(dir = '/home/hunter/FIA/', #dir = here('vol/data/FIA'), 
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

