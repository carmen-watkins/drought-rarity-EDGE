## Header ####
## Script name: Rank Abundance Curves

## Purpose of script: Create rank abundance curves for supplementary info
##
## Author: Carmen Watkins
##

## read in data
source("analyses/calc_response_ratio.R") 

## sort sites into order by precip
edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", 
                                               "SGS", "SBL", "SBK"))

## plot
ggplot(edge_RR, aes(x=spatial_rarity, y=mean.ctrl.cov)) +
  geom_point() +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  geom_vline(xintercept = 0.25) +
  xlab("Spatial Rarity (1-percent rank)") +
  ylab("Mean Cover in Control Plots") +
  theme(text = element_text(size = 13))

## save
ggsave("figures/final/supplement/FigS4_rank_abundance_curve.png", 
       width = 5, height = 6)


ggplot(edge_RR, aes(x=temporal_rarity, y=mean.ctrl.cov)) +
  geom_point() +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  geom_vline(xintercept = 0.25) +
  xlab("Spatial Rarity (1-percent rank)") +
  ylab("Mean Cover in Control Plots") +
  theme(text = element_text(size = 13))

ggplot(edge_RR, aes(x=temporal_rarity)) +
  #geom_histogram() +
  geom_density() +
  #geom_freqpoly() + 
  facet_wrap(~site, ncol = 6, nrow = 1)

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


