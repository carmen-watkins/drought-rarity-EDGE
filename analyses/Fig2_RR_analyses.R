# Header ####
## Script name: Response Ratio Analyses

## Purpose of script: Visualize drought and post-drought response ratios in 
## relation to rarity patterns.
##
## Author: Carmen Watkins
##

# Set up ####
source("analyses/calc_response_ratio.R") 
#source("analyses/color_palettes.R")

library(ggpubr)

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

## get number of unique species in analyses
unique(edge_RR$species)

# Data Mods ####
## Summarise ####
edge_RR_cats = edge_RR %>%
  mutate(spatial = ifelse(spatial_rarity < 0.25, "Sp Common", "Sp Rare"),
         temporal = ifelse(temporal_rarity < 0.5, "Tmp Common", "Tmp Rare"),
         rarity_cat = paste0(temporal, ", ", spatial)) %>%
  
  ## filter out species with ONE observation only.
  ## this was added 2/23/26; CHECK with co-authors before actually including...
  filter(!(site == "KNZ" & species %in% drop1x[drop1x$site == "KNZ",]$species),
         !(site == "HYS" & species %in% drop1x[drop1x$site == "HYS",]$species),
         !(site == "CHY" & species %in% drop1x[drop1x$site == "CHY",]$species),
         !(site == "SGS" & species %in% drop1x[drop1x$site == "SGS",]$species),
         !(site == "SBL" & species %in% drop1x[drop1x$site == "SBL",]$species),
         !(site == "SBK" & species %in% drop1x[drop1x$site == "SBK",]$species))

## arrange sites
edge_RR_cats$site = factor(edge_RR_cats$site, levels = c("KNZ", "HYS", "CHY", 
                                                         "SGS", "SBL", "SBK"))

edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", 
                                               "SBL", "SBK"))

## summary metrics ####
unique(edge_RR$species)

table(edge_RR$site, edge_RR$species)

site_rich = edge_RR %>%
  group_by(site) %>%
  summarise(num_sp = n())

# Figure 2 ####
## MS Figure ####
SR_drought = ggplot(edge_RR, aes(x= spatial_rarity, y=resp.ratio.site_D4, 
                                 color = site)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.9, size = 0.6) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.14", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.21", size = 3, parse = TRUE)

SR_postdrought = ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PDfull, 
                                     color = site)) +
  geom_point(alpha = 0.9, size = 0.6) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.11", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.14", size = 3, parse = TRUE)

TR_drought = ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D4, 
                                  color = site)) +
  geom_point(alpha = 0.9, size = 0.6) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) + 
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.15", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.18", size = 3, parse = TRUE)

TR_postdrought = ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_PDfull, 
                                      color = site)) +
  geom_point(alpha = 0.9, size = 0.6) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.09", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.10", size = 3, parse = TRUE)

ggarrange(SR_drought, TR_drought, SR_postdrought, TR_postdrought,
          labels = c("(a)", "(b)", "(c)", "(d)"), common.legend = T, 
          legend = "bottom", ncol = 2, nrow=2)

ggsave("figures/review_figs/Fig2_resp_ratio_v_rarity.tiff", width = 18, height = 16, units = "cm")

## Talk Figure ####
ggplot(edge_RR, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 20)) +
  coord_cartesian(ylim = c(-1,1)) +
  theme(legend.position="bottom")

## ggsave("figures/dissertation_talk/drr_sr.png", width = 6, height = 4.75)

ggplot(edge_RR, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(ncol=1,byrow=TRUE)) +
  theme(text = element_text(size = 20)) +
  coord_cartesian(ylim = c(-1,1)) +
  theme(legend.position="right")

## ggsave("figures/dissertation_talk/drr_sr_site_lines.png", width = 7, height = 4.75)

ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D4)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
 # geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("Drought Response Ratio") +
  guides(color=guide_legend(ncol=1,byrow=TRUE)) +
  theme(text = element_text(size = 20))

## ggsave("figures/dissertation_talk/drr_tr.png", width = 6, height = 4.75)

ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D4)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  labs(color = "Site") +
  ylab("Drought Response Ratio") +
  guides(color=guide_legend(ncol=1,byrow=TRUE)) +
  theme(text = element_text(size = 20))

## ggsave("figures/dissertation_talk/drr_tr_site_lines.png", width = 7, height = 4.75)

ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PDfull)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(ncol=1,byrow=TRUE)) +
  theme(text = element_text(size = 20))

## ggsave("figures/dissertation_talk/pdrr_sr_site_lines.png", width = 7, height = 4.75)

ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_PDfull)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("Post-drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(ncol=1,byrow=TRUE)) +
  theme(text = element_text(size = 20))

## ggsave("figures/dissertation_talk/pdrr_tr_site_lines.png", width = 7, height = 4.75)
