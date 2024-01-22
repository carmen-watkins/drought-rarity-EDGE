# Header #### 
## Script name: Response Ratio - Drought vs. Recovery
## Purpose of script: Visually explore the relationship b/w response ratio of drought and control plots across years to rank & persistence during two time periods (drought & recovery). Further includes the slope of RR v. rank/persistence by site MAP.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up env ####
library(ggpubr)

## read in response ratio data
source("analyses/calculate_response_ratio.R") 
theme_set(theme_classic())

# Rank v Persistence ####
## create supplementary figure of rank vs. persistence
ggplot(edge_w_predictors.site, aes(x=percrank, y=persistence.site)) +
  geom_point(color = "#363636") +
  facet_wrap(~site) +
  geom_smooth(method = "lm", color = "black", alpha = 0.25) +
  ylab("Persistence") +
  xlab("Rank")

#ggsave("preliminary_figs/rank_vs_persistence.png", width = 6, height = 4)

# Final Figures ####

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c

#798234,#a3ad62,#d0d3a2,#fdfbe4,#f0c6c3,#df91a3,#d46780
#009392,#39b185,#9ccb86,#e9e29c,#eeb479,#e88471,#cf597e

#009B9E,#42B7B9,#A7D3D4,#F1F1F1,#E4C1D9,#D691C1,#C75DAB


#3d5941,#778868,#b5b991,#f6edbd,#edbb8a,#de8a5a,#ca562c
## Fig 1 ####
rank.drought <- ggplot(edge_w_predictors.site, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = site), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#009B9E", "#42B7B9", "#de8a5a","#ca562c","#D691C1", "#C75DAB")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

per.drought <- ggplot(edge_w_predictors.site, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = site), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, size = 0.8) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#009B9E", "#42B7B9", "#de8a5a","#ca562c","#D691C1", "#C75DAB")) +
  xlab("Persistence") +
  ylab("")

rank.recov <- ggplot(edge_w_predictors.site.recov, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = site), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, size = 0.8) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black", size = 1.5) +  #facet_wrap(~site, scales ="free") +
  #geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#009B9E", "#42B7B9", "#de8a5a","#ca562c","#D691C1", "#C75DAB")) +
  xlab("Rank") +
  ylab("Recovery Response Ratio")

per.recov <- ggplot(edge_w_predictors.site.recov, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = site), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, size = 0.8) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black", size = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#009B9E", "#42B7B9", "#de8a5a","#ca562c","#D691C1", "#C75DAB")) +
  xlab("Persistence") +
  ylab("")

ggarrange(rank.drought, per.drought, rank.recov, per.recov, labels = "AUTO", common.legend = T, legend = "bottom")

ggsave("preliminary_figs/resp_ratio_rank_persistence/Fig1_RR_drought_recov_color_update.png", width = 6.5, height = 6.5)

## Fig 2 ####
## Drought RR v Recovery RR
## merge drought & recovery dfs
  ## fix column names before merging
drought.RR <- edge_w_predictors.site %>%
  mutate(resp.ratio.drought = resp.ratio.site,
         mean.cov.drought = mean.cov) %>%
  select(site, species, resp.ratio.drought, mean.cov.drought, persistence.site, percrank)

recov.RR <- edge_w_predictors.site.recov %>%
  mutate(resp.ratio.recov = resp.ratio.site,
         mean.cov.recov = mean.cov) %>%
  select(site, species, resp.ratio.recov, mean.cov.recov)

## merge drought & recov dataframes
response.ratio.tog <- left_join(drought.RR, recov.RR, by = c("site", "species"))

## visualize
ggplot(response.ratio.tog, aes(x=resp.ratio.drought, resp.ratio.recov, color = site)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  geom_smooth(method = "lm", alpha = 0.10, color = "black", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#009B9E", "#42B7B9", "#de8a5a","#ca562c","#D691C1", "#C75DAB"))

ggsave("preliminary_figs/resp_ratio_rank_persistence/DRR_v_RRR.png", width = 5, height = 5)

# RR v. Rank/Persist ####
## During Drought ####
### Persistence ####

ggplot(edge_w_predictors.site, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_drought_site_faceted.png", width = 8, height = 6)

### Rank ####
ggplot(edge_w_predictors.site, aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_drought_site_faceted.png", width = 8, height = 6)

## During Recovery ####
### Persistence ####
ggplot(edge_w_predictors.site.recov, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Recovery Response Ratio (Across Yrs; Site level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_recov_site_faceted.png", width = 8, height = 6)

### Rank ####
ggplot(edge_w_predictors.site.recov, aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Recovery Response Ratio (Across Yrs; Site level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/flipped_RR/rank_RR_recov_site_faceted.png", width = 8, height = 6)
