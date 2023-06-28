# Header #### 
## Script name: Response Ratio - Drought vs. Recovery
##
## Purpose of script: Visually explore the relationship b/w response ratio of drought and control plots across years to rank & persistence during two time periods (drought & recovery). Further includes the slope of RR v. rank/persistence by site MAP.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up env ####
library(ggpubr)

## read in response ratio data
source("analyses/calculate_response_ratio.R") 
map.dat <- read.csv("data/map_data.csv")
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
## Fig 1 ####
rank.drought <- ggplot(edge_w_predictors.site, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Rank") +
  ylab("Drought Response Ratio")

per.drought <- ggplot(edge_w_predictors.site, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("")

rank.recov <- ggplot(edge_w_predictors.site.recov, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +  #facet_wrap(~site, scales ="free") +
  #geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Rank") +
  ylab("Recovery Response Ratio")

per.recov <- ggplot(edge_w_predictors.site.recov, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("")

ggarrange(rank.drought, per.drought, rank.recov, per.recov, labels = "AUTO", common.legend = T, legend = "bottom")

ggsave("preliminary_figs/resp_ratio_rank_persistence/Fig1_RR_drought_recov.png", width = 6.5, height = 6.5)

# Exploratory Figures ####
## Drought v Recovery RR ####
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
ggplot(response.ratio.tog, aes(x=resp.ratio.drought, resp.ratio.recov)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio")
  
#ggsave("preliminary_figs/resp_ratio_rank_persistence/DRR_v_RRR.png", width = 6, height = 4)


## RR Slope v. MAP ####
### Calc Slope ####
## use response.ratio.tog dataframe 
RR.slope <- response.ratio.tog %>%
  group_by(site) %>%
  summarise(slope.drought.rank = lm(resp.ratio.drought~percrank)$coefficients[2],
         slope.drought.persistence = lm(resp.ratio.drought~persistence.site)$coefficients[2],
         slope.recov.rank = lm(resp.ratio.recov~percrank)$coefficients[2],
         slope.recov.persistence = lm(resp.ratio.recov~persistence.site)$coefficients[2])

## join with MAP data
RR.slope.map <- left_join(RR.slope, map.dat, by = c("site"))

## change order of sites
RR.slope.map$site <- as.factor(RR.slope.map$site)
RR.slope.map <- RR.slope.map %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

### Visualize ####
slope.DR <- ggplot(RR.slope.map, aes(x=MAP.mm, y=slope.drought.rank)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("MAP (mm)") +
  ylab("Slope (DRR v. Rank)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black")

slope.RR <- ggplot(RR.slope.map, aes(x=MAP.mm, y=slope.recov.rank)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("MAP (mm)") +
  ylab("Slope (RRR vs. Rank)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black")

slope.DP <- ggplot(RR.slope.map, aes(x=MAP.mm, y=slope.drought.persistence)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("MAP (mm)") +
  ylab("Slope (DRR vs. Persist)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black")

slope.RP <- ggplot(RR.slope.map, aes(x=MAP.mm, y=slope.recov.persistence)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("MAP (mm)") +
  ylab("Slope (RRR vs. Persist)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black")

ggarrange(slope.DR, slope.DP, slope.RR, slope.RP, labels = "AUTO", common.legend = T, 
          legend = "bottom")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/slopes_vs_MAP.png", width = 5, height = 5.5)

## RR v. Rank/Persist ####
### During Drought ####
#### Persistence ####
##### all sites ####
ggplot(edge_w_predictors.site, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_drought.png", width = 5, height = 4)

##### sites faceted ####
ggplot(edge_w_predictors.site, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site, scales = "free") +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_drought_site_faceted.png", width = 8, height = 6)

#### Rank ####
##### all sites ####
ggplot(edge_w_predictors.site, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Percent Rank") +
  ylab("Drought Response Ratio")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_drought.png", width = 5, height = 4)

##### sites faceted ####
ggplot(edge_w_predictors.site, aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Drought Response Ratio (Across Yrs; Site level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_drought_site_faceted.png", width = 8, height = 6)

### During Recovery ####
#### Persistence ####
##### all sites ####
ggplot(edge_w_predictors.site.recov, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio (Across Yrs)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR.png", width = 5, height = 4)

##### sites faceted ####
ggplot(edge_w_predictors.site.recov, aes(x=persistence.site, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence (Site level)") +
  ylab("Recovery Response Ratio (Across Yrs; Site level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/persistence_RR_recov_site_faceted.png", width = 8, height = 6)

#### Rank ####
##### all sites ####
ggplot(edge_w_predictors.site.recov, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = site)) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1) +
  geom_smooth(method = "lm", alpha = 0.05, color = "black") +  #facet_wrap(~site, scales ="free") +
  #geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank") +
  ylab("Recovery Response Ratio (Across Yrs)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_recov.png", width = 5, height = 4)

##### sites faceted ####
ggplot(edge_w_predictors.site.recov, aes(x=percrank, y=resp.ratio.site, color = site)) +
  geom_point() +
  facet_wrap(~site) +
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("% Rank (Site level)") +
  ylab("Recovery Response Ratio (Across Yrs; Site level)")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/flipped_RR/rank_RR_recov_site_faceted.png", width = 8, height = 6)


