## Create Response Ratio Figures
library(ggpubr)

## Set up env
source("analyses/calculate_response_ratio.R")
theme_set(theme_bw())

# Final Mods ####

edge_yearly_w_years <- left_join(edge_yearly_w_predictors, year.key, by = c("site", "experiment.year", "treatment.year"))

edge_yearly_spei_predictors <- left_join(edge_yearly_w_years, spei.exp, by = c("site", "year")) 

edge_yearly_spei_predictors$site <- as.factor(edge_yearly_spei_predictors$site)
edge_yearly_spei_predictors <- edge_yearly_spei_predictors %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

# Response Ratio Across Years ####
## During Drought ####
### Persistence Site ####
#### all sites ####
per.site <- ggplot(edge_w_predictors.site, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_flipped_site.png", width = 5, height = 4)

#### sites faceted ####
ggplot(edge_w_predictors.site, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_flipped_site_faceted.png", width = 8, height = 6)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

### Persistence Block ####
#### all sites ####
#ggplot(edge_w_predictors.block, aes(x=persistence.plot, y=resp.ratio.block, color = site)) +
  #geom_point() +
  #facet_wrap(~site, scales ="free") +
  #geom_smooth(method = "lm", alpha = 0.25) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  #scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  #xlab("Persistence (Block level)") +
  #ylab("Drought Response Ratio (Across Yrs; Block level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_block.png", width = 5, height = 4)

#### sites faceted ####
#ggplot(edge_w_predictors.block, aes(x=persistence.plot, y=resp.ratio.#block, color = site)) +
  #geom_point() +
  #facet_wrap(~site) +
#  geom_smooth(method = "lm", alpha = 0.25) +
 # geom_hline(yintercept = 0, linetype = "dashed") +
  #scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
#  xlab("Persistence (Block level)") +
 # ylab("Drought Response Ratio (Across Yrs; Block level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_block_faceted.png", width = 8, height = 6)

### Rank Site ####
#### all sites ####
rank.site <- ggplot(edge_w_predictors.site, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Percent Rank") +
  ylab("Drought Response Ratio")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_flipped_site.png", width = 5, height = 4)

##### Fig 1 ####
ggarrange(rank.site, per.site, labels = "AUTO", common.legend = T, legend = "bottom")

ggsave("preliminary_figs/resp_ratio_rank_persistence/DRR_flipped_rank_persistence.png", width = 6.5, height = 4)

#### sites faceted ####
ggplot(edge_w_predictors.site, aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_flipped_site_faceted.png", width = 8, height = 6)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

### Rank Block ####
#### all sites ####
#ggplot(edge_w_predictors.block, aes(x=percrank, y=resp.ratio.block, color = site)) +
 # geom_point() +
  #facet_wrap(~site, scales ="free") +
  #geom_smooth(method = "lm", alpha = 0.25) +
#  geom_hline(yintercept = 0, linetype = "dashed") +
 # scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  #xlab("% Rank (Site level)") +
  #ylab("Drought Response Ratio (Across Yrs; Block level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_block.png", width = 5, height = 4)

#### sites faceted ####
#ggplot(edge_w_predictors.block, aes(x=percrank, y=resp.ratio.block, color = site)) +
 # geom_point() +
  #facet_wrap(~site, scales ="free") +
#  geom_smooth(method = "lm", alpha = 0.25) +
 # geom_hline(yintercept = 0, linetype = "dashed") +
  #scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
#  xlab("% Rank (Site level)") +
 # ylab("Drought Response Ratio (Across Yrs; Block level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_block_faceted.png", width = 8, height = 6)

## During Recovery ####
### Persistence Site ####
#### all sites ####
ggplot(edge_w_predictors.site.recov, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  #facet_wrap(~site, scales ="free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Recovery Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_flipped_site_recov_years.png", width = 5, height = 4)

#### sites faceted ####
ggplot(edge_w_predictors.site.recov, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Recovery Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_flipped_site_faceted_recov_years.png", width = 8, height = 6)

### Rank Site ####
#### all sites ####
ggplot(edge_w_predictors.site.recov, aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  #facet_wrap(~site, scales ="free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Recovery Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_flipped_site_recov_years.png", width = 5, height = 4)

#### sites faceted ####
ggplot(edge_w_predictors.site.recov, aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Recovery Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_flipped_site_faceted_recov_years.png", width = 8, height = 6)

# Yearly Response Ratio ####
ggplot(edge_yearly_w_predictors, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~experiment.year, ncol = 5, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_flipped_site_yearly.png", width = 10, height = 6)

ggplot(edge_yearly_w_predictors, aes(x=persistence.site, y=resp.ratio.site, color = treatment.year)) +
  geom_point() +
  facet_wrap(~site, ncol = 5, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)")

## Drought Years Only ####
## Sites faceted
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$treatment.year == "drought",], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  labs(color = "Drought Year")


ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_flipped_site_Dyears_yearly.png", width = 10, height = 6)

## Sites faceted
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$treatment.year == "drought",], aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  labs(color = "Drought Year")

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_flipped_site_Dyears_yearly.png", width = 10, height = 6)

## Recov years only ####
## Sites faceted
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$treatment.year == "recovery",], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#de8a5a", "#edbb8a","#b4c8a8","#70a494","#008080" )) +
  xlab("Persistence (Site level)") +
  ylab("Recovery Response Ratio (Site level)") +
  labs(color = "Recovery Year")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_site_Ryears_yearly.png", width = 10, height = 6)

## Sites faceted
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$treatment.year == "recovery",], aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#de8a5a", "#edbb8a","#b4c8a8","#70a494","#008080" )) +
  xlab("% Rank (Site level)") +
  ylab("Recovery Response Ratio (Site level)") +
  labs(color = "Recovery Year")

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_site_Ryears_yearly.png", width = 10, height = 6)



## Persistence Indiv Years ####
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 0,], aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")


ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 1,], aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")


ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 2,], aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 3,], aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 4,], aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")


## Rank Indiv Years ####
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 0,], aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")


ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 1,], aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")


ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 2,], aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 3,], aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$experiment.year == 4,], aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")


## Sites Indiv, Persistence ####
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "KNZ" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("KNZ")

ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "HYS" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("HYS")

ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "CHY" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("CHY")


ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "SEV_blue" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("SEV_blue")


ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "SEV_black" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("SEV_black")

## Sites Indiv, Rank ####
ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "KNZ" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Rank (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("KNZ")

ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "HYS" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Rank (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("HYS")

ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "CHY" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("CHY")


ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "SEV_blue" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("SEV_blue")


ggplot(edge_yearly_w_predictors[edge_yearly_w_predictors$site == "SEV_black" & edge_yearly_w_predictors$experiment.year %in% c(0:4),], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  #facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)") +
  ggtitle("SEV_black")


# Yearly w/SPEI ####
ggplot(edge_yearly_spei_predictors, aes(x=percrank, y=resp.ratio.site, color = as.factor(spei.class))) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25)



ggplot(edge_yearly_spei_predictors[edge_yearly_spei_predictors$spei < 0,], aes(x=percrank, y=resp.ratio.site, color = as.factor(year))) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm")

ggplot(edge_yearly_spei_predictors[edge_yearly_spei_predictors$spei > 0,], aes(x=percrank, y=resp.ratio.site, color = as.factor(year))) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm")

ggplot(edge_yearly_spei_predictors[edge_yearly_spei_predictors$site == "SEV_black",], aes(x=percrank, y=resp.ratio.site, color = as.factor(year))) +
  geom_point() +
  facet_wrap(~treatment.year*spei.class) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", alpha = 0.15) #+
  #facet_wrap(~treatment.year) #+
 # scale_color_manual(values = c("#d0587e", "#e5b9ad", "#b1c7b3", "#009392"))

#009392,#72aaa1,#b1c7b3,#f1eac8,#e5b9ad,#d98994,#d0587e

ggplot(edge_yearly_spei_predictors[edge_yearly_spei_predictors$site == "SEV_blue",], aes(x=percrank, y=resp.ratio.site, color = as.factor(spei.class))) +
  geom_point() +
  #facet_wrap(~spei.class) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", alpha = 0.15) +
  scale_color_manual(values = c("#d0587e", "#e5b9ad", "#b1c7b3", "#009392"))
  
ggplot(edge_yearly_spei_predictors, aes(x=percrank, y=resp.ratio.site, color = as.factor(spei.class))) +
  geom_point() +
  #facet_wrap(~spei.class) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", alpha = 0.15) +
  scale_color_manual(values = c("#d0587e", "#e5b9ad", "#b1c7b3", "#009392")) +
  facet_wrap(~site)


ggplot(edge_yearly_spei_predictors, aes(x=percrank, y=resp.ratio.site, color = as.factor(spei.class.exp.years))) +
  geom_point() +
  #facet_wrap(~spei.class) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", alpha = 0.15) +
  scale_color_manual(values = c("#d0587e", "#e5b9ad", "#b1c7b3", "#009392")) +
  facet_wrap(~site)


ggplot(edge_yearly_spei_predictors, aes(x=spei.class.exp.years, y=spei.class)) +
  geom_point()
