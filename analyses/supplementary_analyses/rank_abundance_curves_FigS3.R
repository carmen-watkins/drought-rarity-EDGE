## Header ####
## Script name: Rank Abundance Curves

## Purpose of script: Create rank abundance curves for supplementary info
##
## Author: Carmen Watkins
##

## read in data
source("analyses/calc_response_ratio.R") 

## sort sites into order by precip
edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## plot
ggplot(edge_RR, aes(x=spatial_rarity, y=mean.ctrl.cov)) +
  geom_point() +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  geom_vline(xintercept = 0.25) +
  xlab("Spatial Rarity (1-percent rank)") +
  ylab("Mean Cover in Control Plots") +
  theme(text = element_text(size = 13))

## save
## ggsave("figures/final/supplement/FigS4_rank_abundance_curve.png", width = 5, height = 6)
