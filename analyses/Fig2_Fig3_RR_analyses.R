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
## wes_palette("Royal3") ## old color palette

## get number of unique species in analyses
unique(edge_RR$species)

# Data Mods ####
## Summarise ####
edge_RR_cats = edge_RR %>%
  mutate(spatial = ifelse(spatial_rarity < 0.25, "Sp Common", "Sp Rare"),
         temporal = ifelse(temporal_rarity < 0.5, "Tmp Common", "Tmp Rare"),
         rarity_cat = paste0(temporal, ", ", spatial)) 

## arrange sites
edge_RR_cats$site = factor(edge_RR_cats$site, levels = c("KNZ", "HYS", "CHY", 
                                                         "SGS", "SBL", "SBK"))

edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", 
                                               "SBL", "SBK"))

# Figure S3 ####
## MS Figure ####
ggplot(edge_RR, aes(x = spatial_rarity, y=temporal_rarity))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
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

## Talk Figure ####
ggplot(edge_RR, aes(x = spatial_rarity, y=temporal_rarity))+
  geom_point(size = 1.5) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 16)) +
  geom_abline(slope = 1, intercept = 0)

## ggsave("figures/dissertation_talk/trare_srare_nolines.png", width = 5, height = 4)

ggplot(edge_RR, aes(x = spatial_rarity, y=temporal_rarity))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0.25, color = "red", linetype = "dashed") +
  geom_point(size = 1.5) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 16)) +
  geom_abline(slope = 1, intercept = 0)

## ggsave("figures/dissertation_talk/trare_srare_lines.png", width = 5, height = 4)


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
  coord_cartesian(ylim = c(-1,1))

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
  theme(text = element_text(size = 13))

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
  theme(text = element_text(size = 13))

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
  theme(text = element_text(size = 13))

ggarrange(SR_drought, TR_drought, SR_postdrought, TR_postdrought,
          labels = c("(a)", "(b)", "(c)", "(d)"), common.legend = T, 
          legend = "bottom", ncol = 2, nrow=2)

# ggsave("figures/review_figs/Fig2_resp_ratio_v_rarity.tiff", width = 6, height = 5.5)

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

# Figure 3 ####
edge_RR_cats %>%
  mutate(rarity_2 = ifelse(rarity_cat == "Tmp Rare, Sp Common", 
                           "Common (S), Intermittent (T)",
                           ifelse(rarity_cat == "Tmp Rare, Sp Rare", 
                                  "Sparse (S), Intermittent (T)", 
                                  ifelse(rarity_cat == "Tmp Common, Sp Common", 
                                         "Common (S), Persistent (T)", 
                                         "Sparse (S), Persistent (T)"))), 
         
         rarity_2 = fct_relevel(rarity_2, "Common (S), Persistent (T)", 
                                "Sparse (S), Persistent (T)",  
                                "Common (S), Intermittent (T)", 
                                "Sparse (S), Intermittent (T)")) %>%
  
ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 2.5, pch = 21) +
  
  facet_wrap(~rarity_2, nrow = 2, ncol = 2) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  labs(fill = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(text = element_text(size = 13)) +
  theme(legend.position = "right") 

## ggsave("figures/final/Fig3_DRR_v_PDRR.tiff", width = 6, height = 5.5)

## summarise categories ####
CIsp = edge_RR_cats %>%
  filter(rarity_cat == "Tmp Rare, Sp Common")

## get number of samples in Common, Intermittent category
cats_sum = edge_RR_cats %>%
  group_by(rarity_cat) %>%
  summarise(numcat = n())

CI_cat = edge_RR_cats %>%
  mutate(rarity_2 = ifelse(rarity_cat == "Tmp Rare, Sp Common", 
                           "Common (S), Intermittent (T)",
                           ifelse(rarity_cat == "Tmp Rare, Sp Rare", 
                                  "Sparse (S), Intermittent (T)", 
                                  ifelse(rarity_cat == "Tmp Common, Sp Common", 
                                         "Common (S), Persistent (T)", 
                                         "Sparse (S), Persistent (T)")))) %>%
  group_by(rarity_cat)


## write.csv(edge_RR_cats, "../edge_RR_and_rarity_categories.csv")
