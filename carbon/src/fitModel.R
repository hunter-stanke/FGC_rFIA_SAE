##=====================================================
##=====================================================
##
## This script fits a spatial Fay-Herriot model to 
## direct estimates of forest carbon stocks (produced in
## `directEstimates.R`), using long-term climate normals
## as predictors (downloaded from the PRISM dataset in 
## `getPRISM.R`). Resulting smoothed estimates (EBPLUP)
## will be saved as `smoothed_estimates.csv`, and stored
## in the `carbon/results/` directory.
##
## Last modified: 22 July 2021 - Hunter Stanke
##
##====================================================
##====================================================

## Load packages/ set working directory ----------------------------------------
library(sae)
library(sf)
library(dplyr)
library(here)



## Read spatial data -----------------------------------------------------------
counties <- sf::st_read(here::here('carbon/data/counties_climate/')) %>%
  sf::st_transform(crs = 'ESRI:102008') %>%
  ## Compute area of each county, sq m --> ha
  dplyr::mutate(ha = as.numeric(sf::st_area(.) / 10000)) %>%
  dplyr::mutate(STATEFP = as.numeric(STATEFP),
                COUNTYFP = as.numeric(COUNTYFP)) %>%
  # Take log of mean annual precipitation
  dplyr::mutate(map = log(map)) %>%
  # Center and scale ppt and tmean
  dplyr::mutate(mat = scale(mat, na.rm = TRUE),
                map = scale(map, na.rm = TRUE))



## Read direct estimates -------------------------------------------------------
pop.est <- read.csv(here::here('carbon/results/direct_estimates.csv')) %>%
  # Convert population totals to population means by dividing by area of units
  dplyr::left_join(as.data.frame(counties), by = c('STATEFP', 'COUNTYFP')) %>%
  ## Results is mean C02e (tons) of forest carbon across all lands
  mutate(carb = CARB_TOTAL / ha / (12/44),
         carb.var = CARB_TOTAL_VAR / (ha^2) / ((12/44)^2)) %>%
  ## Compute relative standard error of post-stratified estimates
  mutate(carb.rse = sqrt(carb.var) / carb)
  



## Build a proximity matrix ----------------------------------------------------
# Function to identify neighbors using rook method
st_rook <- function(a, b = a) st_relate(a, b, pattern = "F***1****")

## Sparse binary predicate --> dense binary matrix
prox.mat <- counties %>% 
  filter(as.numeric(COUNTYNS) %in% pop.est$COUNTYNS) %>%
  mutate(NB_QUEEN = st_rook(.))
prox.mat <- as.matrix(prox.mat$NB_QUEEN)

## Binary matrix to proximity matrix required by `sae`
for (i in 1:nrow(prox.mat)) {
  ## Number of neighbors
  nn <- sum(prox.mat[i,])
  ## Weight each neighbor equally
  prox.mat[i,] <- prox.mat[i,] / nn
}

## Replace NAs w/ zero
prox.mat[is.na(prox.mat)] <- 0



## Fit Fay-Herriot area-level model --------------------------------------------
# A design matrix
design.mat <- cbind(as.factor(pop.est$COUNTYNS),
                    pop.est$ppt,
                    pop.est$tmean)
# Fit the model
mod <- mseSFH(carb ~ design.mat, 
              vardir = carb.var,
              proxmat = prox.mat,
              method = 'REML',
              data = pop.est)




## Save results ----------------------------------------------------------------
# Appending EBLUP estimates to dataframe w/ post-stratified estimates
pop.est$pred <- mod$est$eblup
pop.est$pred.mse <- mod$mse
pop.est$pred.rse <- sqrt(pop.est$pred.mse) / pop.est$pred

# Ratio of smoothed RMSE to post-stratified SE
pop.est$ser <- sqrt(pop.est$pred.mse) / sqrt(pop.est$carb.var)

# Save to file
write.csv(dplyr::select(pop.est, COUNTYNS,
                        carb, carb.var, carb.rse, 
                        pred, pred.mse, pred.rse,
                        ser),
          here('carbon/results/smoothed_estimates.csv'),
          row.names = FALSE)
