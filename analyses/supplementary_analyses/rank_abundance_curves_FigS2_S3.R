## Header ####
## Script name: Rarity Correlations and Rank Abundance Curves

## Purpose of script: Create rank abundance curves for supplementary info
## visualise histograms of temporal rarity also to pair with RACs
##
## Author: Carmen Watkins
##

# Set up ####
library(ggpubr)

## read in data
source("analyses/calc_response_ratio.R") 

## sort sites into order by precip
edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", 
                                               "SGS", "SBL", "SBK"))


## create df of correlation values between rarity types.
edge_cor = edge_RR %>%
  group_by(site) %>%
  summarise(rare_corr = round(cor(spatial_rarity, temporal_rarity, 
                            method = c("pearson")), 3)) %>%
  mutate(x = rep(0.8, 6),
         y = rep(0.1, 6))

# Figure S1 ####
ggplot(edge_RR, aes(x = spatial_rarity, y=temporal_rarity))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0.25, color = "red", linetype = "dashed") +
  geom_point(size = 0.75) +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 11)) +
  geom_text(data = edge_cor, mapping = aes(x = x, y = y, label = rare_corr),
            size = 3.25) +
  scale_x_continuous(breaks = c(0,1,0.5),
                     labels = c(0, 1, 0.5)) +
  scale_y_continuous(breaks = c(0,1,0.5),
                     labels = c(0, 1, 0.5))

ggsave("figures/final_figs/supp_figs/FigS1_rarity_correlation.png", 
       width = 8, height = 10, units = "cm")

# Figure S2 ####
## rank abundance curve (spatial rarity functions as rank as it is 
## calculated as: 1 - percent rank)
a = ggplot(edge_RR, aes(x=spatial_rarity, y=mean.ctrl.cov)) +
  geom_point() +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  geom_vline(xintercept = 0.25, linetype = "dashed") +
  geom_vline(xintercept = 0.15, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.35, linetype = "dashed", color = "red") +
  xlab("Spatial Rarity") +
  ylab("Mean Cover") +
  theme(text = element_text(size = 12))  +
  scale_x_continuous(breaks = c(0,1,0.5),
                     labels = c(0, 1, 0.5))

## plot histograms of temporal rarity
b = ggplot(edge_RR, aes(x=temporal_rarity)) +
  geom_histogram() +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0.4, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "red") +
  xlab("Temporal Rarity") +
  ylab("Count") +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  theme(text = element_text(size = 12))  +
  scale_x_continuous(breaks = c(0,1,0.5),
                     labels = c(0, 1, 0.5))

## plot density pots of temporal rarity
c = ggplot(edge_RR, aes(x=temporal_rarity)) +
  geom_density() +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0.4, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0.6, linetype = "dashed", color = "red") +
  xlab("Temporal Rarity") +
  ylab("Density") +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  theme(text = element_text(size = 12))  +
  scale_x_continuous(breaks = c(0,1,0.5),
                     labels = c(0, 1, 0.5))

ggarrange(a, b, c, ncol = 1, labels = c("a", "b", "c"))

## save
ggsave("figures/final_figs/supp_figs/FigS2_rank_abundance_curve.png", 
 width = 18, height = 15, units = "cm")

## test what temp rarity vs. abundance looks like
#ggplot(edge_RR, aes(x=temporal_rarity, y=mean.ctrl.cov)) +
 # geom_point() +
  #facet_wrap(~site, ncol = 6, nrow = 1) +
#  geom_vline(xintercept = 0.5, linetype = "dashed") +
 # xlab("Temporal Rarity") +
  #ylab("Mean Cover in Control Plots") +
#  theme(text = element_text(size = 12)) +
 # scale_x_continuous(breaks = c(0,1,0.5),
  #                   labels = c(0, 1, 0.5))
#ggsave("figures/review_figs/for_response/temp_rarity_v_cover.png", width = 10, 
 #      height = 3)  

## test what histogram of spatial rarity looks like
#ggplot(edge_RR, aes(x=spatial_rarity)) +
 # geom_histogram() +
  #facet_wrap(~site, ncol = 6, nrow = 1)
## much more evenly spread, which makes sense as the percent rank function is 
## standardizes differences in abundance

