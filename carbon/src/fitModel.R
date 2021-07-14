

## Load packages/ set working directory ----------------------------------------
library(sae)
library(sf)
library(dplyr)
library(here)



## Read spatial data -----------------------------------------------------------
counties <- st_read(here('carbon/data/GIS/counties')) %>%
  st_transform(crs = 'ESRI:102008') %>%
  ## Compute area of each, sq m --> ha
  mutate(ha = as.numeric(st_area(.) / 10000)) %>%
  mutate(COUNTYNS = as.numeric(COUNTYNS))



## Read direct estimates -------------------------------------------------------
pop.est <- read.csv(here('carbon/results/direct_estimates.csv')) %>%
  # Slim it down
  dplyr::select(COUNTYNS, CARB_TOTAL, CARB_TOTAL_VAR, nPlots_TREE, N) %>%
  # Convert population totals to population means by dividing by area of units
  left_join(dplyr::select(as.data.frame(counties), COUNTYNS, ha), by = 'COUNTYNS') %>%
  ## Results is mean C02e (tons) of forest carbon across all lands
  mutate(carb = CARB_TOTAL / ha / (12/44),
         carb.var = CARB_TOTAL_VAR / (ha^2) / ((12/44)^2)) %>%
  ## Compute coefficient of variation of post-stratified estimates
  mutate(carb.cv = sqrt(carb.var) / carb)


## Read climate data -----------------------------------------------------------
pop.est <- pop.est %>%
  left_join(read.csv(here('carbon/results/climate_county.csv')),
            by = 'COUNTYNS') %>%
  # An interaction term for precip and tmean
  mutate(ppt.x.tmean = ppt * tmean) %>%
  # Scale our predictors
  mutate(ppt = log(ppt)) %>%
  # Center and scale ppt and tmean, but only scale 
  # their interaction (reduce correlation among predictors)
  mutate(across(ppt:tmean, .fns = function(x) {(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}),
         ppt.x.tmean = ppt.x.tmean / sd(ppt.x.tmean, na.rm = TRUE))



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
pop.est$pred.cv <- sqrt(pop.est$pred.mse) / pop.est$pred

# Ratio of smoothed RMSE to post-stratified SE
pop.est$ser <- sqrt(pop.est$pred.mse) / sqrt(pop.est$carb.var)

# Save to file
write.csv(dplyr::select(pop.est, COUNTYNS,
                        carb, carb.var, carb.cv, 
                        pred, pred.mse, pred.cv,
                        ser, cvr),
          here('carbon/results/smoothed_estimates.csv'),
          row.names = FALSE)
