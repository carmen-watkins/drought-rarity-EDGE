# Header #### 
## Script name: Response Ratio - Drought vs. Recovery
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

# Final Mods ####
## join edge cover data with year key
edge_yearly_w_years <- left_join(edge_yearly_w_predictors, year.key, by = c("site", "experiment.year", "treatment.year"))

## now join with spei data
edge_yearly_spei_predictors <- left_join(edge_yearly_w_years, spei.exp, by = c("site", "year")) 

## change order of sites to match ppt gradient
edge_yearly_spei_predictors$site <- as.factor(edge_yearly_spei_predictors$site)
edge_yearly_spei_predictors <- edge_yearly_spei_predictors %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

# Yearly Response Ratio ####
## Rank ####
ggplot(edge_yearly_w_predictors, aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Rank") +
  ylab("Response Ratio") +
  labs(color= "Exp Year")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_yearly_site_faceted.png", width = 8, height = 6)

## Persistence ####
ggplot(edge_yearly_w_predictors, aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Persistence") +
  ylab("Response Ratio") +
  labs(color= "Exp Year")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_yearly_site_faceted.png", width = 8, height = 6)

# Drought Years Only ####
## Rank ####
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$treatment.year == "drought",], aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  labs(color = "Drought Year")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_yearly_drought_site_faceted.png", width = 10, height = 6)

## Persistence ####
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$treatment.year == "drought",], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  labs(color = "Drought Year")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_yearly_drought_site_faceted.png", width = 10, height = 6)

# Recov years only ####
## Rank ####
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$treatment.year == "recovery",], aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#de8a5a", "#edbb8a","#b4c8a8","#70a494","#008080" )) +
  xlab("Rank") +
  ylab("Recovery Response Ratio") +
  labs(color = "Recovery Year")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_yearly_recov_site_faceted.png", width = 10, height = 6)

## Persistence ####
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$treatment.year == "recovery",], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#de8a5a", "#edbb8a","#b4c8a8","#70a494","#008080" )) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio") +
  labs(color = "Recovery Year")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_yearly_recov_site_faceted.png", width = 10, height = 6)

# Yearly w/SPEI ####
## Color by Raw SPEI ####
### Rank ####
ggplot(edge_yearly_spei_predictors, aes(x=percrank, y=resp.ratio.site, color = as.factor(spei))) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Rank") +
  ylab("Response Ratio")

ggsave("preliminary_figs/slope_resp_ratio/rank_RR_yearly_spei.png", width = 12, height = 5)

### Persistence ####
ggplot(edge_yearly_spei_predictors, aes(x=persistence.site, y=resp.ratio.site, color = as.factor(spei))) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Persistence") +
  ylab("Response Ratio")

ggsave("preliminary_figs/slope_resp_ratio/persistence_RR_yearly_spei.png", width = 12, height = 5)

## Slope ####
### RR v. Rank ####
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

### RR v. Persist ####
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
