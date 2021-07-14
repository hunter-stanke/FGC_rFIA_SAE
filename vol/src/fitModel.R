
## Load packages/ set working directory ----------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(brms)


## Read plot summaries ---------------------------------------------------------
dat <- read.csv(here('vol/results/plt_summaries.csv')) %>%
  mutate(STRATUM_CN = as.factor(STRATUM_CN)) %>%
  ## Scale variables
  mutate(time = YEAR - mean(YEAR),
         # Converting ccf/acre to m^3/ha
         vol = BOLE_CF_ACRE / 35.315 * 2.471) 


## Fit model w/ brms -----------------------------------------------------------

# Formulate model
# Regression coefficients vary by plot, stratum (nested random intercept/slope)
# Note that we are assuming a Guassian response, this is not ideal
mod.form <- bf(vol ~ time + (time | pltID:STRATUM_CN) + (time | STRATUM_CN))

# Set priors -- all weakly informative
priors <- c(prior(normal(50, 250), class = Intercept), # Population-level intercept
            prior(normal(0, 100), class = b, coef = time), # Population-level slope
            ## These are constrained to be non-negative by brms
            ## 0 mean indicates a folded student t
            prior(student_t(3, 0, 100), class = sigma), # Residual standard deviation
            prior(student_t(3, 0, 100), class = sd, group = STRATUM_CN), # Stratum-level SD on both intercept and slope
            prior(student_t(3, 0, 100), class = sd, group = pltID:STRATUM_CN) # Plot-level SD on both intercept and slope
)

## Fit model
mod <- brm(mod.form,
           data = dat,
           family = gaussian,
           prior = priors,
           iter = 4000,
           thin = 2,
           control = list(adapt_delta = .99,
                          max_treedepth=15),
           file = here('vol/results/mod.RDS'),
           chains = 3,
           cores = 3,
           backend = "cmdstanr",
           threads = threading(4))


## Predict merchantable volume for all strata (2000-2025) ----------------------

# Set up dataframe w/ strata/years to predict to
newdat.list <- list()
for (i in unique(dat$STRATUM_CN)) {
  newdat.list[[i]] <- data.frame(STRATUM_CN = i,
                               YEAR = 1998:2025,
                               time = 1998:2025 - mean(dat$YEAR))
}
newdat <- bind_rows(newdat.list)


## Extract coefficients from the model
alpha <- coef(mod, summary = FALSE)$STRATUM_CN[,,1] %>% 
  t() %>% 
  as.data.frame() %>%
  `names<-`(paste0('iter', 1:3000)) %>%
  mutate(STRATUM_CN = as.factor(row.names(.))) %>%
  pivot_longer(cols = -c(STRATUM_CN), names_to = 'iter', values_to = 'alpha') %>%
  mutate(iter = stringr::str_sub(iter, 5, -1))
beta <- coef(mod, summary = FALSE)$STRATUM_CN[,,2] %>%
  t() %>% 
  as.data.frame() %>%
  `names<-`(paste0('iter', 1:3000)) %>%
  mutate(STRATUM_CN = as.factor(row.names(.))) %>%
  pivot_longer(cols = -c(STRATUM_CN), names_to = 'iter', values_to = 'beta') %>%
  mutate(iter = stringr::str_sub(iter, 5, -1))


## Clean table of estimated coefficients at every iteration
params <- left_join(alpha, beta, by = c('STRATUM_CN', 'iter')) 


## Construct estimator of population total
totals <- newdat %>% 
  ## Join regression coefficients and design information 
  left_join(params, by = c('STRATUM_CN')) %>%
  left_join(distinct(select(dat, ESTN_UNIT_CN, AREA_USED, STRATUM_CN, STRATUM_WGT)), by = 'STRATUM_CN') %>%
  ## Weighted average across strata
  ## Equivalent to imputing the mean to all population units at time t
  group_by(YEAR, time, iter, ESTN_UNIT_CN, AREA_USED) %>%
  summarize(alpha = sum(alpha * STRATUM_WGT),
            beta = sum(beta * STRATUM_WGT)) %>%
  ## Convert to population total for each estimation unit
  mutate(alpha = alpha * AREA_USED,
         beta = beta * AREA_USED) %>%
  ## Sum across estimation units
  group_by(YEAR, time, iter) %>%
  summarize(alpha = sum(alpha),
            beta = sum(beta)) %>%
  ungroup() %>%
  ## Predict
  mutate(BOLE_CM_TOTAL = alpha + beta * time)



## Save predictions ------------------------------------------------------------
write.csv(totals, here('vol/results/predicted_totals.csv'), row.names = FALSE)


