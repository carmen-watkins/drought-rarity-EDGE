## Header ####
## Script name: Rank Abundance Curves

## Purpose of script: Create rank abundance curves for supplementary info
##
## Author: Carmen Watkins
##

library(ggpubr)

## read in data
source("analyses/calc_response_ratio.R") 

## sort sites into order by precip
edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", 
                                               "SGS", "SBL", "SBK"))

## plot
a = ggplot(edge_RR, aes(x=spatial_rarity, y=mean.ctrl.cov)) +
  geom_point() +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  #geom_vline(xintercept = 0.1) +
  geom_vline(xintercept = 0.15, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.35, linetype = "dashed", color = "red") +
  xlab("Spatial Rarity") +
  ylab("Mean Cover") +
  theme(text = element_text(size = 12))  +
  scale_x_continuous(breaks = c(0,1,0.5),
                     labels = c(0, 1, 0.5))

b = ggplot(edge_RR, aes(x=temporal_rarity)) +
  geom_histogram() +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0.4, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "red") +
  xlab("Temporal Rarity") +
  ylab("Count") +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  scale_x_continuous(breaks = c(0,1,0.5),
                     labels = c(0, 1, 0.5))

c = ggplot(edge_RR, aes(x=temporal_rarity)) +
  geom_density() +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0.4, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "red") +
  xlab("Temporal Rarity") +
  ylab("Density") +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  scale_x_continuous(breaks = c(0,1,0.5),
                     labels = c(0, 1, 0.5))

ggarrange(a, b, c, ncol = 1, labels = c("(a)", "(b)", "(c)"))

## save
ggsave("figures/review_figs/FigS2_rank_abundance_curve.tiff", 
 width = 18, height = 15, units = "cm")







ggplot(edge_RR, aes(x=temporal_rarity, y=mean.ctrl.cov)) +
  geom_point() +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  geom_vline(xintercept = 0.25) +
  xlab("Spatial Rarity (1-percent rank)") +
  ylab("Mean Cover in Control Plots") +
  theme(text = element_text(size = 13))
hist(edge_RR$temporal_rarity)

sites = unique(edge_RR$site)
# Set up 1 row, 3 columns
par(mfrow = c(1, 6))

# Loop through each unique group and plot
for(s in sites) {
  subset_data = subset(edge_RR, site == s)
  hist(subset_data$temporal_rarity)

}



  
ggplot(edge_RR, aes(x=spatial_rarity)) +
  geom_histogram() +
  facet_wrap(~site, ncol = 6, nrow = 1)


  geom_point() +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  geom_vline(xintercept = 0.25) +
  xlab("Spatial Rarity (1-percent rank)") +
  ylab("Mean Cover in Control Plots") +
  theme(text = element_text(size = 13))


