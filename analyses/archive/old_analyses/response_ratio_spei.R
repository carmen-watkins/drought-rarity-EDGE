# Header #### 
## Script name: Response Ratio SPEI
##
## Purpose of script: Visually explore the relationship b/w response ratio of drought and control plots to rank & persistence yearly & by SPEI
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up Env ####
library(ggpubr)

## read in response ratio calcs
source("analyses/calculate_response_ratio.R")
source("data-prep/clean_spei_data.R") ## read in spei data
theme_set(theme_classic())

# Color by Raw SPEI ####
## Rank ####
ggplot(edge_yearly_spei_predictors, aes(x=percrank, y=resp.ratio.site, color = as.factor(spei))) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Rank") +
  ylab("Response Ratio")

ggsave("preliminary_figs/slope_resp_ratio/rank_RR_yearly_spei.png", width = 12, height = 5)

## Persistence ####
ggplot(edge_yearly_spei_predictors, aes(x=persistence.site, y=resp.ratio.site, color = as.factor(spei))) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Persistence") +
  ylab("Response Ratio")

ggsave("preliminary_figs/slope_resp_ratio/persistence_RR_yearly_spei.png", width = 12, height = 5)

# Slope ####
## RR v. Rank ####
## absolute cover
slope.rank.RR <- edge_yearly_spei_predictors %>%
  group_by(site, year, spei) %>%
  summarise(slope = lm(resp.ratio.site~percrank)$coefficients[2])

ggplot(slope.rank.RR, aes(x=spei, slope)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope of Response Ratio vs. Rank") +
  xlab("SPEI") +
  geom_smooth(method = "lm", alpha = 0.25)

ggsave("preliminary_figs/slope_resp_ratio/slope_RR_rank_by_spei.png", width = 6, height = 4)

## RR v. Persist ####
slope.persist.RR <- edge_yearly_spei_predictors %>%
  group_by(site, year, spei) %>%
  summarise(slope = lm(resp.ratio.site~persistence.site)$coefficients[2])

ggplot(slope.persist.RR, aes(x=spei, slope)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Slope of Response Ratio vs. Persistence") +
  xlab("SPEI") +
  geom_smooth(method = "lm", alpha = 0.25)

ggsave("preliminary_figs/slope_resp_ratio/slope_RR_persistence_by_spei.png", width = 6, height = 4)
