##=====================================================
##=====================================================
##
## This script contains all code necessary to download
## and process prism climate data as used in
## ... . Data are downloaded from a public
## repository, and preprocessed for subsequent use.
##
## Last modified: 22 June 2021 - Hunter Stanke
##
##====================================================
##====================================================


## Load packages/ set working directory ----------------------------------------
library(prism)
library(stars)
library(sf)
library(dplyr)
library(stringr)
library(here)




##  Download PRISM data ---------------------------------------------------------
# Where to store it?
options(prism.path = here('carbon/data/PRISM/'))

# Mean ppt -- 30-year normals
get_prism_normals(type = "ppt", resolution = '800m', annual = TRUE, keepZip = FALSE)

# Mean temp -- 30-year normals
get_prism_normals(type = "tmean", resolution = '800m', annual = TRUE, keepZip = FALSE)



## Load PRISM data -------------------------------------------------------------
# Precip normals
ppt <- read_stars(here('carbon/data/PRISM/PRISM_ppt_30yr_normal_800mM2_annual_bil/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil'))

# Tmean normals
tmean <- read_stars(here('carbon/data/PRISM/PRISM_tmean_30yr_normal_800mM2_annual_bil/PRISM_tmean_30yr_normal_800mM2_annual_bil.bil'))




## Aggregate into means within counties ----------------------------------------
# County shapefile
counties <- st_read(here('carbon/data/GIS/counties/')) %>%
  st_transform(crs = st_crs(ppt))


## A function to aggregate climate variables within spatial units
agg.clim <- function(X, ppt, tmean, polys) {
  
  ## Pull out individual spatial unit
  shp <- polys[X,]
  
  ## Crop rasters to extent of spatial unit
  suppressMessages({
    ppt.crop <- st_crop(ppt, shp)
    tmean.crop <- st_crop(tmean, shp)
  })

  ## Take the mean of each variable and store in data.frame
  out <- data.frame(COUNTYNS = shp$COUNTYNS, 
                    ppt = mean(ppt.crop$PRISM_ppt_30yr_normal_800mM2_annual_bil.bil, na.rm = TRUE),
                    tmean = mean(tmean.crop$PRISM_tmean_30yr_normal_800mM2_annual_bil.bil, na.rm = TRUE))
  
  return(out)
}


## Do the aggregation 
out <- lapply(X = 1:nrow(counties), FUN = agg.clim, 
                ppt, tmean, counties)

## Back to dataframe
clim <- bind_rows(out)



## Save ecoregion shapefile with aggregated climate variables ------------------
write.csv(clim, here('carbon/results/climate_county.csv'), row.names = FALSE)

