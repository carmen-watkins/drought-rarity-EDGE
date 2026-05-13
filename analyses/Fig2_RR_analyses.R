# Header ####
## Script name: Response Ratio Analyses

##' Purpose of script: Plot drought and post-drought response ratios in 
##' relation to rarity patterns. Figure 2 in Watkins et al. 2026.
##
## Author: Carmen Watkins
##

# Set up ####
## load data
source("analyses/calc_response_ratio.R") 

## load packages
library(ggpubr)

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70", "#CE685E", "#E5AA7F", "#FCD484")

# Data Mods ####
## arrange sites
edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", 
                                               "SBL", "SBK"))

# Figure 2 ####
## spatial, drought
SR_drought = ggplot(edge_RR, aes(x= spatial_rarity, y=resp.ratio.site_D4, 
                                 color = site)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.9, size = 1) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.38)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.14", size = 3.5, 
           parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.21", size = 3.5,
           parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.87 [0.67, 1.08]", 
           size = 3.5, parse = FALSE)

## spatial, post-drought
SR_postdrought = ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PDfull, 
                                     color = site)) +
  geom_point(alpha = 0.9, size = 1) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.38)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.11", size = 3.5, 
           parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.14", size = 3.5, 
           parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.76 [0.56, 0.97]",
           size = 3.5, parse = FALSE)

## temporal, drought
TR_drought = ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D4, 
                                  color = site)) +
  geom_point(alpha = 0.9, size = 1) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) + 
  coord_cartesian(ylim = c(-1,1.38)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.15", size = 3.5,
           parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.18", size = 3.5, 
           parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.72 [0.55, 0.89]",
           size = 3.5, parse = FALSE)

## temporal, post-drought
TR_postdrought = ggplot(edge_RR, aes(x=temporal_rarity,
                                     y=resp.ratio.site_PDfull, color = site)) +
  geom_point(alpha = 0.9, size = 1) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.38)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.09", size = 3.5, 
           parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.10", size = 3.5,
           parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.57 [0.40, 0.74]",
           size = 3.5, parse = FALSE)

## arrange
plot = ggarrange(SR_drought, TR_drought, SR_postdrought, TR_postdrought,
          labels = c("a", "b", "c", "d"), common.legend = T, 
          legend = "bottom", ncol = 2, nrow=2)

annotate_figure(plot, top = text_grob("Figure 2", 
                                      color = "black", face = "bold", 
                                      size = 14))

## save official version
# ggsave("figures/final_figs/Fig2_resp_ratio_v_rarity.tiff", width = 18, 
## height = 16.5, units = "cm", bg="white", dpi = 600)
