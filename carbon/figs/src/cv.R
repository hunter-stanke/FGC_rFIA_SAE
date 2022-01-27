library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(here)


## Read smoothed estimates -----------------------------------------------------
sm <- read.csv(here('carbon/results/smoothed_estimates.csv')) %>%
  arrange(carb.cv) %>%
  mutate(id = 1:n())



plt <- sm %>% 
  filter(pred.cv < 1.15) %>%
  select(id, carb.cv, pred.cv) %>%
  tidyr::pivot_longer(cols = c(carb.cv, pred.cv), names_to = 'Estimator', values_to = 'cv') %>%
  mutate(Estimator = case_when(Estimator == 'carb.cv' ~ 'Post-stratified periodic',
                          TRUE ~ 'Spatial Fay-Herriot model')) %>%
  ggplot(aes(x= id, y = cv*100, group = Estimator, colour = Estimator, shape = Estimator)) +
  geom_point(alpha = .1, size = 0.9) +
  theme_bw() +
  scale_colour_brewer(palette = 'Dark2') +
  xlab('Counties (ordered smallest to largest RSE Direct)') +
  ylab('Relative Standard Error (%)') +
  guides(colour = guide_legend(override.aes = c(alpha = 1, size = 2))) +
  scale_y_continuous(breaks = seq(0, 110, 10)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = c(.325,.8),
        legend.title = element_text(size = 8, face = 'bold.italic'),
        legend.text = element_text(size = 8, face = 'italic'),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = .25))
plt

ggsave(plt, filename = here('carbon/figs/carbon_cv.pdf'), height = 3, width = 3.5)
ggsave(plt, filename = here('carbon/figs/figure3.jpg'), height = 3, width = 3.5)
