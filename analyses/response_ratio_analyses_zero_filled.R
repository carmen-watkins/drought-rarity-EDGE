# Header ####
## Script name: Response Ratio Analyses

## Purpose of script: Visualize drought and post-drought response ratios in relation to rarity patterns.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
source("analyses/calc_response_ratio.R") 
source("analyses/color_palettes.R")

#library(viridisLite)
#library(scico)
#library(cowplot)
library(ggpubr)
#library(viridis)
#library(ggExtra)
#library(wesanderson)

## set up graphics
theme_set(theme_classic())
pal <- wes_palette("Royal3")
wes_palette("Royal3")

## get number of unique species in analyses
unique(edge_RR$species)
## 293

# Data Mods ####
## Summarise ####
edge_RR_cats = edge_RR %>%
  mutate(spatial = ifelse(spatial_rarity < 0.25, "Abundant", "Scarce"),
         temporal = ifelse(temporal_rarity > 0.5, "Core", "Transient"),
         rarity_cat = paste0(temporal, ", ", spatial),
         MAP_level = ifelse(site %in% c("KNZ", "HYS"), "High", 
                            ifelse(site %in% c("CHY", "SGS"), "Intermediate", "Low"))) 

## arrange rarity categories
edge_RR_cats$rarity_cat = factor(edge_RR_cats$rarity_cat, levels = c("Transient, Abundant", "Transient, Scarce", "Core, Abundant", "Core, Scarce"))

edge_RR_cats$site = factor(edge_RR_cats$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## Summary DF ####
#category_sums = edge_RR %>%
 # group_by(MAP_level, rarity_cat) %>%
  #summarise(num = n()) %>%
  #ungroup() %>%
#  group_by(MAP_level) %>%
 # mutate(tot = sum(num)) %>%
  #ungroup() %>%
  #mutate(perc = num/tot)

# Figure 2 ####
ggplot(edge_RR, aes(x = spatial_rarity, y=temporal_rarity))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  geom_vline(xintercept = 0.25, color = "red", linetype = "dashed") +
  geom_point(size = 1.5) +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 13))
## ggsave("figures/Jan2025/figure2_site.png", width = 10, height = 2.5)

## check correlations ####
### KNZ
cor(edge_RR[edge_RR$site == "KNZ",]$spatial_rarity, edge_RR[edge_RR$site == "KNZ",]$temporal_rarity, method = c("pearson"))
### HYS
cor(edge_RR[edge_RR$site == "HYS",]$spatial_rarity, edge_RR[edge_RR$site == "HYS",]$temporal_rarity, method = c("pearson"))
### CHY
cor(edge_RR[edge_RR$site == "CHY",]$spatial_rarity, edge_RR[edge_RR$site == "CHY",]$temporal_rarity, method = c("pearson"))
### SGS
cor(edge_RR[edge_RR$site == "SGS",]$spatial_rarity, edge_RR[edge_RR$site == "SGS",]$temporal_rarity, method = c("pearson"))
### SBL
cor(edge_RR[edge_RR$site == "SBL",]$spatial_rarity, edge_RR[edge_RR$site == "SBL",]$temporal_rarity, method = c("pearson"))
### SBK
cor(edge_RR[edge_RR$site == "SBK",]$spatial_rarity, edge_RR[edge_RR$site == "SBK",]$temporal_rarity, method = c("pearson"))

# Figure 3 ####
SR_drought <- ggplot(edge_RR, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1.75) +
  #geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15))

SR_postdrought <- ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PDfull)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1.75) +
 # geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15))

TR_drought <- ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D4)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1.75) +
  #geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15))

TR_postdrought <- ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_PDfull)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1.75) +
 # geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15))

ggarrange(SR_drought, TR_drought, SR_postdrought, TR_postdrought,
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 2, nrow=2)

#ggsave("figures/Jan2025/resp_ratio_v_rarity.png", width = 10, height = 8.5)

# Figure 4 ####
ggplot(edge_RR_cats, aes(x=resp.ratio.site_D, y=resp.ratio.site_PD, color = MAP_level)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2) +
  facet_wrap(~rarity_cat, nrow = 2, ncol = 2) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  labs(color = "Relative MAP") +
  #geom_smooth(method = "lm", alpha = 1)+
  scale_color_manual(values = c(pal[1], pal[4], pal[6])) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")

#ggsave("figures/Nov2024_meeting/figure4_zero_filled.png", width = 7, height = 7)

