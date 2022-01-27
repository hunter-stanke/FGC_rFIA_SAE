library(ggplot2)
library(dplyr)
library(here)

## Read annual model-based estimates
preds <- read.csv(here('vol/results/predicted_totals.csv')) %>%
  mutate(BOLE_CM_TOTAL = BOLE_CM_TOTAL / 1e6) %>%
  group_by(YEAR) %>%
  mutate(m = median(BOLE_CM_TOTAL),
         lower = quantile(BOLE_CM_TOTAL, .025),
         upper = quantile(BOLE_CM_TOTAL, .975),
         sd = sd(BOLE_CM_TOTAL)) %>%
  mutate(cv = sd / m * 100) %>%
  mutate(type = 'Model-based estimator')


## Read annual design-based estimates
panels <- read.csv( here('vol/results/annual_panels.csv')) %>%
  mutate(BOLE_CM_TOTAL = BOLE_CF_TOTAL / 1e6 / 35.315 * 2.471) %>%
  mutate(BOLE_CM_TOTAL_VAR = BOLE_CF_TOTAL_VAR / (1e6)^2 / (35.315)^2 * (2.471)^2) %>%
  mutate(halfint = qt(0.975, df = N-1) * sqrt(BOLE_CM_TOTAL_VAR)) %>%
  mutate(cv = sqrt(BOLE_CM_TOTAL_VAR) / BOLE_CM_TOTAL * 100) %>%
  mutate(type = 'Post-stratified annual estimator')


## Plot to compare model and design-based
plt <- preds %>%
  
  ggplot(aes(x = YEAR, y = BOLE_CM_TOTAL)) +
  
  ## Simulated lines
  geom_line(aes(group = iter), alpha = .01, colour = 'blue') +
  ## 95% credible intervals
  geom_line(aes(x = YEAR, y = upper), linetype = 2, colour = 'blue') +
  geom_line(aes(x = YEAR, y = lower), linetype = 2, colour = 'blue') +
  geom_line(aes(x = YEAR, y = m), colour = 'blue', size = .8) +
  
  ## Panel points and CI
  geom_segment(data = panels, aes(x = YEAR, xend = YEAR,
                                  y = BOLE_CM_TOTAL - halfint, yend = BOLE_CM_TOTAL + halfint),
               colour = 'grey10', alpha = .5, size = .8)+
  geom_line(data = panels, aes(x = YEAR, y = BOLE_CM_TOTAL),
            colour = 'grey10') +
  geom_point(data = panels, aes(x = YEAR, y = BOLE_CM_TOTAL),
             colour = 'grey10',
             fill = 'grey95',
             shape = 21,
             size = 3) +
  
  geom_curve(aes(x = 2002, y = 157.4, xend = 2007.5, yend = 144.8),
               arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
               colour = 'grey10') +
  
  geom_curve(aes(x = 2020, y = 87.5, xend = 2022.5, yend = 120.5),
               arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
               colour = 'blue', curvature = .2) +
  
  annotate('label', x = 2002, y = 157.4, label = 'Post-stratified\nannual estimator', colour = 'grey10') +
  annotate('label', x = 2020, y = 87.5, label = 'Model-based\n estimator', colour = 'blue') +
  
  
  theme_bw() +
  xlab('') +
  #scale_y_continuous(limits = c(1.1, 2.3), breaks = c(1.25, 1.5, 1.75, 2, 2.25)) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  ylab('Merchantable bole volume\n(million cubic meters)') 
  #labs(title = "20-year trends in merchantable volume",
  #     subtitle = 'Washington Survey Unit, Maine')
plt

ggsave(plt, filename = here('vol/figs/me_volume_trends.pdf'), height = 7.25, width = 7.25)
ggsave(plt, filename = here('vol/figs/figure4.jpg'), height = 7.25, width = 7.25)



## Plot to compare model and design-based CVs
cv <- preds %>%
  select(YEAR, type, cv) %>%
  bind_rows(select(panels, YEAR, type, cv)) %>%
  mutate(Estimator = factor(type, levels = c('Post-stratified annual estimator', 'Model-based estimator'))) %>%
  ggplot(aes(x = YEAR, y = cv, group = Estimator, colour = Estimator, shape = Estimator)) +
  geom_line(alpha = .25, size = .25) +
  geom_point(size = 1.5, alpha = .9) +
  theme_bw() +
  scale_colour_brewer(palette = 'Dark2') +
  ylab('Relative Standard Error (%)') +
  xlab('') +
  
  #annotate('label', x = 2002, y = 157.4, label = 'Post-stratified\nannual estimator', colour = 'grey10') +
  #annotate('label', x = 2020, y = 87.5, label = 'Model-based\n estimator', colour = 'blue') +
  
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  scale_y_continuous(limits = c(3, 17), breaks = seq(4, 16, 2)) +
  theme(legend.position = c(.675,.75),
        legend.direction = 'vertical',
        legend.title = element_text(face = 'bold.italic', size = 7),
        axis.title = element_text(size = 8),
        axis.text = element_text(size = 7),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', size = .25),
        legend.text = element_text(size = 7, face = 'italic'),
        legend.box.margin = margin(-25))
cv

ggsave(cv, filename = here('vol/figs/me_volume_cv.pdf'), height = 3, width = 3.5)
ggsave(cv, filename = here('vol/figs/figure5.jpg'), height = 3, width = 3.5)
