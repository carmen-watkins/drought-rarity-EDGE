# Header ####
## Script name: Response Ratio Analyses

## Purpose of script: Visualize drought and post-drought response ratios in relation to rarity patterns.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
source("analyses/new_response_ratio_calcs_zero_filled.R") 
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
## 296

# Data Mods ####
## Summarise ####
edge_RR_cats = edge_RR %>%
  mutate(spatial = ifelse(percrank > 0.75, "Abundant", "Scarce"),
         temporal = ifelse(persistence.site > 0.5, "Core", "Transient"),
         rarity_cat = paste0(temporal, ", ", spatial),
         MAP_level = ifelse(site %in% c("KNZ", "HYS"), "High", 
                            ifelse(site %in% c("CHY", "SGS"), "Intermediate", "Low"))) 

## arrange rarity categories
edge_RR_cats$rarity_cat = factor(edge_RR_cats$rarity_cat, levels = c("Transient, Abundant", "Transient, Scarce", "Core, Abundant", "Core, Scarce"))

edge_RR_cats$site = factor(edge_RR_cats$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## Summary DF ####
category_sums = sum_edge_RR %>%
  group_by(MAP_level, rarity_cat) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  group_by(MAP_level) %>%
  mutate(tot = sum(num)) %>%
  ungroup() %>%
  mutate(perc = num/tot)

# Figure 2 ####
ggplot(edge_RR, aes(x = percrank, y=persistence.site))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(size = 1.5) +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse()

ggsave("figures/Nov2024_meeting/figure2_site.png", width = 6.5, height = 8)

## check correlations ####
cor(sum_edge_RR[sum_edge_RR$site == "KNZ",]$rank, sum_edge_RR[sum_edge_RR$site == "KNZ",]$persistence, method = c("pearson"))
cor(sum_edge_RR[sum_edge_RR$site == "HYS",]$rank, sum_edge_RR[sum_edge_RR$site == "HYS",]$persistence, method = c("pearson"))
cor(sum_edge_RR[sum_edge_RR$site == "CHY",]$rank, sum_edge_RR[sum_edge_RR$site == "CHY",]$persistence, method = c("pearson"))
cor(sum_edge_RR[sum_edge_RR$site == "SGS",]$rank, sum_edge_RR[sum_edge_RR$site == "SGS",]$persistence, method = c("pearson"))
cor(sum_edge_RR[sum_edge_RR$site == "SBL",]$rank, sum_edge_RR[sum_edge_RR$site == "SBL",]$persistence, method = c("pearson"))
cor(sum_edge_RR[sum_edge_RR$site == "SBK",]$rank, sum_edge_RR[sum_edge_RR$site == "SBK",]$persistence, method = c("pearson"))

cor(sum_edge_RR[sum_edge_RR$MAP_level == "High",]$rank, sum_edge_RR[sum_edge_RR$MAP_level == "High",]$persistence, method = c("pearson"))

cor(sum_edge_RR[sum_edge_RR$MAP_level == "Intermediate",]$rank, sum_edge_RR[sum_edge_RR$MAP_level == "Intermediate",]$persistence, method = c("pearson"))

cor(sum_edge_RR[sum_edge_RR$MAP_level == "Low",]$rank, sum_edge_RR[sum_edge_RR$MAP_level == "Low",]$persistence, method = c("pearson"))


# Figure 3 ####
rankD3 <- ggplot(edge_RR, aes(x= percrank, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought Response Ratio") +
  labs(color = "Relative MAP") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

rankR3 <- ggplot(edge_RR, aes(x=percrank, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Percent Rank") +
  ylab("Post-drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persD3 <- ggplot(edge_RR, aes(x=persistence.site, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persR3 <- ggplot(edge_RR, aes(x=persistence.site, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Persistence") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

meanD3 <- ggplot(edge_RR, aes(x=mean.ctrl.cov, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

meanR3 <- ggplot(edge_RR, aes(x=mean.ctrl.cov, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Mean Cov (space/time)") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

logmeanD3 = ggplot(edge_RR, aes(x=log(mean.ctrl.cov), y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

logmeanR3 <- ggplot(edge_RR, aes(x=log(mean.ctrl.cov), y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Log Mean Cov (space/time)") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

absrankD3 = ggplot(edge_RR, aes(x=absrank, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

absrankR3 <- ggplot(edge_RR, aes(x=absrank, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Absolute Rank") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

ggarrange(rankD3, absrankD3, persD3, meanD3, logmeanD3, rankR3, absrankR3, persR3, meanR3, logmeanR3,
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 5, nrow=2)

#ggsave("figures/Nov2024_meeting/figure3_expanded.png", width = 16, height = 8.5)

## Figure S5 ####
siterankD3 <- ggplot(sum_edge_RR, aes(x= rank, y=meanDRR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought Response Ratio") +
  labs(color = "Relative MAP") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

siterankR3 <- ggplot(sum_edge_RR, aes(x=rank, y=meanPDRR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

sitepersD3 <- ggplot(sum_edge_RR, aes(x=persistence, y=meanDRR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

sitepersR3 <- ggplot(sum_edge_RR, aes(x=persistence, y=meanPDRR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 2) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

ggarrange(siterankD3, sitepersD3, siterankR3, sitepersR3, 
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 2, nrow=2)

#ggsave("figures/final_figs/supp/figures5.tiff", width = 10, height = 8.5)

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

ggsave("figures/Nov2024_meeting/figure4_zero_filled.png", width = 7, height = 7)

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

ggsave("figures/Nov2024_meeting/figure4_alt_zero_filled.png", width = 9, height = 4)

#88CCEE,#CC6677,#DDCC77,#117733,#332288,#AA4499,#44AA99,#999933,#882255,#661100,#6699CC,#888888


ggplot(sum_edge_RR, aes(x=meanDRR, y=meanPDRR, color = rarity_cat)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2) +
  # facet_wrap(~MAP_level, nrow = 1, ncol = 3) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  labs(color = "Rarity Category") +
  geom_smooth(method = "lm", alpha = 0.2)+
  scale_color_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(text = element_text(size = 15)) +
  theme(legend.position = "bottom")






