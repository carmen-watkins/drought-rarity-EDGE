# Header #### 
## Script name: MAP vs. Response Ratio
##
## Purpose of script: Visually explore the relationship b/w response ratio of drought and control plots across years to the site mean annual precipitation (MAP)
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


# RR Slope v. MAP ####
## Calc Slope ####
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

## Visualize ####
slope.DR <- ggplot(RR.slope.map, aes(x=MAP.mm, y=slope.drought.rank)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("MAP (mm)") +
  ylab("Slope (DRR v. Rank)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  coord_cartesian(ylim = c(-2,1))

slope.RR <- ggplot(RR.slope.map, aes(x=MAP.mm, y=slope.recov.rank)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("MAP (mm)") +
  ylab("Slope (RRR vs. Rank)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  coord_cartesian(ylim = c(-2,1))

slope.DP <- ggplot(RR.slope.map, aes(x=MAP.mm, y=slope.drought.persistence)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("MAP (mm)") +
  ylab("Slope (DRR vs. Persist)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  coord_cartesian(ylim = c(-2,1))

slope.RP <- ggplot(RR.slope.map, aes(x=MAP.mm, y=slope.recov.persistence)) +
  geom_point(size = 3, aes(color = site)) +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("MAP (mm)") +
  ylab("Slope (RRR vs. Persist)") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  coord_cartesian(ylim = c(-2,1))

ggarrange(slope.DR, slope.DP, slope.RR, slope.RP, labels = "AUTO", common.legend = T, 
          legend = "bottom")

ggsave("preliminary_figs/resp_ratio_rank_persistence/slopes_vs_MAP.png", width = 5, height = 5.5)