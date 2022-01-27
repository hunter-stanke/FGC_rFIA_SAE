##=====================================================
##=====================================================
##
## This script fits a Bayesian mixed-effects model to 
## plot-level summaries of merchantable wood volume 
## density (volume / acre) (produced in`pltSummaries.R`), 
## yielding a temporally explicit model of the state 
## variable in Maine. Resulting estimates are derived 
## summaries of MCMC samples, saved as`predicted_totals.csv`, 
## and stored in the `vol/results/` directory.
##
## Last modified: 22 July 2021 - Hunter Stanke
##
##====================================================
##====================================================


## Load packages/ set working directory ----------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(brms)


## Read plot summaries ---------------------------------------------------------
dat <- read.csv(here::here('vol/results/plt_summaries.csv')) %>%
  dplyr::mutate(STRATUM_CN = as.factor(STRATUM_CN)) %>%
  ## Scale variables
  dplyr::mutate(time = YEAR - 1999,
                # Converting ccf/acre to m^3/ha
                vol = BOLE_CF_ACRE / 35.315 * 2.471) 


## Fit model w/ brms -----------------------------------------------------------

# Formulate model
# Regression coefficients vary by plot, stratum (nested random intercept/slope)
# Note that we are assuming a Guassian response, this is not ideal
mod.form <- brms::bf(vol ~ time + (time | pltID:STRATUM_CN) + (time | STRATUM_CN))

# Set priors -- all weakly informative
priors <- c(
  # Population-level regression coeffients
  brms::prior(normal(50, 250), 
              class = Intercept), 
  brms::prior(normal(0, 100), 
              class = b,
              coef = time), 
  
  # Among- and within stratum standard deviation of coefficients
  # These cover both alpha and beta
  brms::prior(student_t(3, 0, 100), 
              class = sd, 
              group = STRATUM_CN), # Across strata
  brms::prior(student_t(3, 0, 100),
              class = sd, 
              group = pltID:STRATUM_CN), # Within stratum
  
  # Residual standard deviation
  brms::prior(student_t(3, 0, 100), 
              class = sigma)
)

## Fit model
mod <- brms::brm(mod.form,
                 data = dat,
                 family = gaussian,
                 prior = priors,
                 iter = 4000,
                 thin = 2,
                 control = list(adapt_delta = .99,
                                max_treedepth=15),
                 file = here::here('vol/results/mod.RDS'),
                 chains = 3,
                 backend = "cmdstanr")


## Extract coefficients from the model
alpha <- coef(mod, summary = FALSE)$STRATUM_CN[,,1] %>% 
  t() %>% 
  as.data.frame() %>%
  `names<-`(paste0('iter', 1:3000)) %>%
  dplyr::mutate(STRATUM_CN = as.factor(row.names(.))) %>%
  tidyr::pivot_longer(cols = -c(STRATUM_CN), names_to = 'iter', values_to = 'alpha') %>%
  dplyr::mutate(iter = stringr::str_sub(iter, 5, -1))
beta <- coef(mod, summary = FALSE)$STRATUM_CN[,,2] %>%
  t() %>% 
  as.data.frame() %>%
  `names<-`(paste0('iter', 1:3000)) %>%
  dplyr::mutate(STRATUM_CN = as.factor(row.names(.))) %>%
  tidyr::pivot_longer(cols = -c(STRATUM_CN), names_to = 'iter', values_to = 'beta') %>%
  dplyr::mutate(iter = stringr::str_sub(iter, 5, -1))


## Clean table of estimated coefficients at every iteration
params <- left_join(alpha, beta, by = c('STRATUM_CN', 'iter')) 


## Simlified survey design info with stratum and 
## estimation unit areas
design.info <- dat %>%
  dplyr::select(ESTN_UNIT_CN, AREA_USED,
                STRATUM_CN, STRATUM_WGT) %>%
  dplyr::distinct()

## Adjust population-level coefficients with design weights
adj.coef <- params %>%
  dplyr::left_join(design.info, by = 'STRATUM_CN') %>%
  ## Weighted average across strata
  ## Equivalent to imputing the mean to 
  ## all population units at time t
  dplyr::group_by(iter, ESTN_UNIT_CN, AREA_USED) %>%
  dplyr::summarize(alpha = sum(alpha * STRATUM_WGT),
                   beta = sum(beta * STRATUM_WGT)) %>%
  ## Convert to population total for each estimation unit
  dplyr::mutate(alpha = alpha * AREA_USED,
                beta = beta * AREA_USED) %>%
  ## Sum across estimation units
  dplyr::group_by(iter) %>%
  dplyr::summarize(alpha = sum(alpha),
                   beta = sum(beta)) %>%
  dplyr::ungroup() 



## Predict merchantable volume for full spatial domain over 1999-2025
pred.list <- list()
for (i in 1999:2025) {
  pred.list[[as.character(i)]] <- adj.coef %>%
    dplyr::mutate(YEAR = i,
                  BOLE_CM_TOTAL = alpha + beta * (i - 1999))
}
totals <- dplyr::bind_rows(pred.list)



## Save predictions ------------------------------------------------------------
write.csv(totals, here('vol/results/predicted_totals.csv'), row.names = FALSE)


