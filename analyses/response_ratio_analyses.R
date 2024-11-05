# Header ####
## Script name: Response Ratio Analyses

## Purpose of script: Visualize drought and post-drought response ratios in relation to rarity patterns.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
source("analyses/calculate_response_ratio.R") 
source("analyses/color_palettes.R")

library(viridisLite)
library(scico)
library(cowplot)
library(ggpubr)
library(viridis)
library(ggExtra)
#library(wesanderson)

## set up graphics
theme_set(theme_classic())
pal <- wes_palette("Royal3")
wes_palette("Royal3")

## get number of unique species in analyses
unique(edge_RR$species)
## 293

# Data Mods ####
## arrange sites in df
edge_RR$site <- factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## Categorize species ####
edge_RR_cats = edge_RR %>%
  mutate(spatial = ifelse(percrank > 0.5, "Abundant", "Scarce"),
         temporal = ifelse(persistence.site > 0.5, "Core", "Transient"),
         rarity_cat = paste0(temporal, ", ", spatial),
         MAP_level = ifelse(site %in% c("KNZ", "HYS"), "High", 
                            ifelse(site %in% c("CHY", "SGS"), "Intermediate", "Low"))) 

## arrange rarity categories
edge_RR_cats$rarity_cat <- factor(edge_RR_cats$rarity_cat, levels = c("Transient, Abundant", "Transient, Scarce", "Core, Abundant", "Core, Scarce"))

## Summary DF ####
category_sums = edge_RR_cats %>%
  group_by(MAP_level, rarity_cat) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  group_by(MAP_level) %>%
  mutate(tot = sum(num)) %>%
  ungroup() %>%
  mutate(perc = num/tot)

# Figure 2 ####
ggplot(edge_RR_cats, aes(x=percrank, y=persistence.site))+
  geom_hline(yintercept = 0.5, color = "gray") +
  geom_vline(xintercept = 0.5, color = "gray") +
  geom_point(size = 1.5) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse()

ggsave("final_figs/figure2.tiff", width = 8.25, height = 3)

## check correlations ####
cor(edge_RR_cats[edge_RR_cats$site == "KNZ",]$percrank, edge_RR_cats[edge_RR_cats$site == "KNZ",]$persistence.site, method = c("pearson"))
cor(edge_RR_cats[edge_RR_cats$site == "HYS",]$percrank, edge_RR_cats[edge_RR_cats$site == "HYS",]$persistence.site, method = c("pearson"))
cor(edge_RR_cats[edge_RR_cats$site == "CHY",]$percrank, edge_RR_cats[edge_RR_cats$site == "CHY",]$persistence.site, method = c("pearson"))
cor(edge_RR_cats[edge_RR_cats$site == "SGS",]$percrank, edge_RR_cats[edge_RR_cats$site == "SGS",]$persistence.site, method = c("pearson"))
cor(edge_RR_cats[edge_RR_cats$site == "SBL",]$percrank, edge_RR_cats[edge_RR_cats$site == "SBL",]$persistence.site, method = c("pearson"))
cor(edge_RR_cats[edge_RR_cats$site == "SBK",]$percrank, edge_RR_cats[edge_RR_cats$site == "SBK",]$persistence.site, method = c("pearson"))

# Figure 3 ####
rankD3 <- ggplot(edge_RR_cats, aes(x=percrank, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = MAP_level), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(pal[1], pal[4], pal[6])) +
  xlab(" ") +
  ylab("Drought Response Ratio") +
  labs(color = "Relative MAP") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

rankR3 <- ggplot(edge_RR_cats, aes(x=percrank, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = MAP_level), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(pal[1], pal[4], pal[6])) +
  xlab("Spatial Rarity") +
  ylab("Post-drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persD3 <- ggplot(edge_RR_cats, aes(x=persistence.site, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = MAP_level), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(pal[1], pal[4], pal[6])) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persR3 <- ggplot(edge_RR_cats, aes(x=persistence.site, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = MAP_level), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(pal[1], pal[4], pal[6])) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

ggarrange(rankD3, persD3, rankR3, persR3, 
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 2, nrow=2)

ggsave("final_figs/figure3.tiff", width = 10, height = 8.5)

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

ggsave("final_figs/figure4.tiff", width = 7, height = 7)

## Fig 4 Alternative ####
ggplot(edge_RR_cats, aes(x=resp.ratio.site_D, y=resp.ratio.site_PD, color = rarity_cat)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2) +
  facet_wrap(~MAP_level, nrow = 1, ncol = 3) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  labs(color = "Rarity Category") +
  #geom_smooth(method = "lm", alpha = 1)+
  scale_color_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")

ggsave("final_figs/figure4_alt.tiff", width = 9, height = 4)

#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#44AA99,#999933,#882255,#661100,#6699CC,#888888
