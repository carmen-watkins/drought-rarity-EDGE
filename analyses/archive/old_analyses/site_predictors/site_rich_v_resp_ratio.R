# Header #### 
## Script name: Site richness vs. Response Ratio
##
## Purpose of script: Visually explore the relationship b/w response ratio of drought and control plots across years to the site mean richness
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

## RR Slope v. Site Rich ####
rich <- edge_all %>%
  group_by(site) %>%
  summarize(SR=length(unique(species,na.rm=T)))

RR.slope.rich <- left_join(RR.slope.map, rich, by = c("site"))

### Visualize ####
slope.DR.rich <- ggplot(RR.slope.rich, aes(x=SR, y=slope.drought.rank)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Richness") +
  ylab("Slope (DRR v. Rank)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  coord_cartesian(ylim = c(-2,0.75))

slope.RR.rich <- ggplot(RR.slope.rich, aes(x=SR, y=slope.recov.rank)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Richness") +
  ylab("Slope (RRR vs. Rank)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  coord_cartesian(ylim = c(-2,0.75))

slope.DP.rich <- ggplot(RR.slope.rich, aes(x=SR, y=slope.drought.persistence)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Richness") +
  ylab("Slope (DRR vs. Persist)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  coord_cartesian(ylim = c(-2,0.75))

slope.RP.rich <- ggplot(RR.slope.rich, aes(x=SR, y=slope.recov.persistence)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Richness") +
  ylab("Slope (RRR vs. Persist)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  coord_cartesian(ylim = c(-2,0.75))

ggarrange(slope.DR.rich, slope.DP.rich, slope.RR.rich, slope.RP.rich, labels = "AUTO", common.legend = T, 
          legend = "bottom")

ggsave("preliminary_figs/resp_ratio_rank_persistence/slopes_vs_rich.png", width = 5, height = 5.5)


