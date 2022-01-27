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
# Where should we save it?
options(prism.path = here::here('carbon/data/PRISM/'))

# Mean ppt -- 30-year normals
prism::get_prism_normals(type = "ppt", 
                         resolution = '800m',
                         annual = TRUE, 
                         keepZip = FALSE)

# Mean temp -- 30-year normals
prism::get_prism_normals(type = "tmean", 
                         resolution = '800m', 
                         annual = TRUE, 
                         keepZip = FALSE)



## Load PRISM data -------------------------------------------------------------
# Precip normals
ppt <- stars::read_stars(here::here('carbon/data/PRISM/PRISM_ppt_30yr_normal_800mM3_annual_bil/PRISM_ppt_30yr_normal_800mM3_annual_bil.bil'))

# Tmean normals
tmean <- stars::read_stars(here::here('carbon/data/PRISM/PRISM_tmean_30yr_normal_800mM3_annual_bil/PRISM_tmean_30yr_normal_800mM3_annual_bil.bil'))



## Download shapefile of county boundaries from TIGRIS dataset -----------------
counties <- tigris::counties(state = lower48) %>%
  # Re-project to match climate data
  sf::st_transform(crs = sf::st_crs(ppt))


## Aggregate climate raster data into county-level averages --------------------
# In our spatial county dataset, each row represents a unique county. Hence, 
# we can iterate ("loop") over rows in the county dataset, extract the 
# climate data that overlaps with the county, and summarize the climate data
# into a series of descriptive statistics (mean, sd, etc). Here we will only 
# save the mean of average annual temperature and precipitation. 

# We'll save our summaries in these vectors, "growing" them as we iterate
county.temp <- c()
county.ppt <- c()
for (i in 1:nrow(counties)) {
  
  ## Crop our climate rasters down to the county boundary
  tmean.crop <- sf::st_crop(tmean, counties[i,])
  ppt.crop <- sf::st_crop(ppt, counties[i,])
  
  ## Save mean temp and ppt
  county.temp <- c(county.temp, mean(tmean.crop[[1]], na.rm = TRUE))
  county.ppt <- c(county.ppt, mean(ppt.crop[[1]], na.rm = TRUE))
  
}

## Now we add our climate summaries to our spatial county dataset --------------
counties$mat <- county.temp # Mean annual temperature
counties$map <- county.ppt # Mean annual precipitation


## Simplify structure of spatial data and save ---------------------------------
counties <- counties %>%
  dplyr::select(STATEFP, COUNTYFP, NAME, mat, map)
sf::write_sf(counties, here::here('carbon/data/counties_climate/counties_climate.shp'))



