
## visualize d4 vs. d7 response ratios to show no difference between the two.

# Set up ####
source("analyses/calc_response_ratio.R") 
source("analyses/color_palettes.R")

library(ggpubr)

theme_set(theme_classic())
pal <- wes_palette("Royal3")

# Figure S4 ####
SR_d = ggplot(edge_RR[edge_RR$site %in% c("SBK", "SBL"),], aes(x= spatial_rarity)) +
  geom_smooth(aes(color = site, y = resp.ratio.site_D4), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(aes(color = site, y = resp.ratio.site_D6), method = "lm", alpha = 0.1, linewidth = 0.75, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(pal[5], pal[6])) +
  xlab("Spatial Rarity") +
  ylab("Drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1))

TR_d = ggplot(edge_RR[edge_RR$site %in% c("SBK", "SBL"),], aes(x=temporal_rarity)) +
  geom_smooth(aes(color = site, y = resp.ratio.site_D4), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(aes(color = site, y = resp.ratio.site_D6), method = "lm", alpha = 0.1, linewidth = 0.75, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c(pal[5], pal[6])) +
  xlab("Temporal Rarity") +
  ylab(" ") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13))

ggarrange(SR_d, TR_d, labels = "AUTO", common.legend = T, legend = "bottom")

#ggsave("figures/Mar2025/FigS4_resp_ratio_v_rarity.png", width = 6, height = 3.5)


