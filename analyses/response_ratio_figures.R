## Create Response Ratio Figures

## Set up env
source("calculate_response_ratio.R")
theme_set(theme_bw())

# Response Ratio Across Years ####
## Persistence Site ####
### all sites ####
ggplot(edge_w_predictors.site, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  #facet_wrap(~site, scales ="free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_site.png", width = 5, height = 4)

### sites faceted ####
ggplot(edge_w_predictors.site, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_site_faceted.png", width = 8, height = 6)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

## Persistence Block ####
### all sites ####
ggplot(edge_w_predictors.block, aes(x=persistence.plot, y=resp.ratio.block, color = site)) +
  geom_point() +
  #facet_wrap(~site, scales ="free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Block level)") +
  ylab("Drought Response Ratio (Across Yrs; Block level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_block.png", width = 5, height = 4)

### sites faceted ####
ggplot(edge_w_predictors.block, aes(x=persistence.plot, y=resp.ratio.block, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Block level)") +
  ylab("Drought Response Ratio (Across Yrs; Block level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_block_faceted.png", width = 8, height = 6)

## Rank Site ####
### all sites ####
ggplot(edge_w_predictors.site, aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  #facet_wrap(~site, scales ="free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_site.png", width = 5, height = 4)

### sites faceted ####
ggplot(edge_w_predictors.site, aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_site_faceted.png", width = 8, height = 6)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

## Persistence Block ####
### all sites ####
ggplot(edge_w_predictors.block, aes(x=percrank, y=resp.ratio.block, color = site)) +
  geom_point() +
  #facet_wrap(~site, scales ="free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Block level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_block.png", width = 5, height = 4)

### sites faceted ####
ggplot(edge_w_predictors.block, aes(x=percrank, y=resp.ratio.block, color = site)) +
  geom_point() +
  facet_wrap(~site, scales ="free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Block level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_block_faceted.png", width = 8, height = 6)

# Yearly Response Ratio ####
ggplot(edge_yearly_w_predictors, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~experiment.year, ncol = 5, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Site level)")

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_site_yearly.png", width = 10, height = 6)

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

ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_site_Dyears_yearly.png", width = 10, height = 6)

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

ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_site_Dyears_yearly.png", width = 10, height = 6)


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

