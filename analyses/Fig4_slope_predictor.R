# Header ####
## Script name: Fig 4 Slope Predictor

## Purpose of script: Create figures of linear model coefficients from linear 
## models that test the effect of rarity on response ratio separately at each site
##
## Author: Carmen Watkins
##

# Set Up ####
source("analyses/Q2_pt1_linear_models_by_site.R")

## join model & pred dat ####
spmods_pred = left_join(sp_mods, site_pred_scaled, by = "site") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

tmpmods_pred = left_join(tmp_mods, site_pred_scaled, by = "site") %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

site_pred_scaled = site_pred_scaled %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

# Fig 4 (drought) #####
## spatial ####
### dominance ####
pD1 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Drought") %>%
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Site Level Dominance") +
  ylab("Slope") +
  labs(fill = "Site")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8)) +
  annotate("text", x = 0.47, y=2.6, label = "Slope: 2.97 [-2.96, 8.91]", size = 3.5, parse = FALSE) +
  annotate("text", x = 0.4, y=2.2, label = "R^2: 0.158", size = 3.5, parse = TRUE) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 6))

### temperature ####
pT1 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Drought") %>%
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("MAT (C)") +
  ylab("Slope") +
  labs(fill = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))  +
  annotate("text", x = 10, y=2.6, label = "Slope: 0.60 [0.18, 1.02]", size = 3.5, parse = FALSE) +
  annotate("text", x = 9, y=2.2, label = "R^2: 0.743", size = 3.5, parse = TRUE) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 6))

### precip ####
pP1 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Drought") %>%
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("MAP (mm)") +
  ylab("Slope") +
  labs(fill = "Site") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8)) +
 # annotate("text", x = 375, y=2.6, label = "p: 0.676", size = 3.5, parse = TRUE) +
  annotate("text", x = 600, y=2.8, label = "Spatial Rarity", size = 4.5) +
  annotate("text", x = 500, y=2.34, label = "Slope: 0.15 [-0.76, 1.06]", size = 3.5, parse = FALSE) +
  annotate("text", x = 375, y=2, label = "R^2: -0.19", size = 3.5, parse = TRUE) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 6))

## temporal ####
### dominance ####
pDt = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Drought") %>%
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Site-Level Dominance") +
  ylab(" ") +
  labs(fill = "Site")  +
  geom_smooth(method = "lm", alpha = 0.1, color = "black", 
              linetype = "dashed") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  annotate("text", x = 0.47, y=2.6, label = "Slope: 2.84 [-0.64, 6.33]", size = 3.5, parse = FALSE) +
  annotate("text", x = 0.4, y=2.2, label = "R^2: 0.452", size = 3.5, parse = TRUE) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 6))

### temperature ####
pTt = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Drought") %>%
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("MAT (C)") +
  ylab(" ") +
  labs(fill = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))  +
  #annotate("text", x = 9, y=2.6, label = "p: 0.021", size = 3.5, parse = TRUE) +
  annotate("text", x = 10, y=2.6, label = "Slope: 0.43 [0.10, 0.76]", size = 3.5, parse = FALSE) +
  annotate("text", x = 9, y=2.2, label = "R^2: 0.713", size = 3.5, parse = TRUE) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 6))

### precip ####
pPt = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Drought") %>%
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("MAP (mm)") +
  ylab(" ") +
  labs(fill = "Site") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))  +
  annotate("text", x = 600, y=2.8, label = "Temporal Rarity", size = 4.5) +
  annotate("text", x = 500, y=2.34, label = "Slope: 0.007 [-0.67, 0.69]", size = 3.5, parse = FALSE) +
  annotate("text", x = 375, y=2, label = "R^2: -0.250", size = 3.5, parse = TRUE) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 6))

## combine ####
plot = ggarrange(pP1, pPt,
          pT1, pTt,
          pD1, pDt,
             
          ncol = 2, nrow = 3, common.legend = T, legend = "bottom", 
          labels = c("a", "b", "c", "d", "e", "f"))

annotate_figure(plot, top = text_grob("Figure 4", 
                                      color = "black", face = "bold", size = 14))

## save
ggsave("figures/final_figs/Fig4_site_slopes_predictors_drought_both_rarity.tiff", 
     width = 15, height = 20, units = "cm", bg="white", dpi = 600)

#ggsave("figures/review_figs/Fig4_site_slopes_predictors_drought_both_rarity.png",
 #      width = 15, height = 20, units = "cm")

# Figure S4 ####
ggplot(site_pred_scaled, aes(x=MAP.mm, y=aridity, color = site)) +
  geom_point(aes(fill = site), colour = "black", size = 3, pch = 21) +
  scale_fill_manual(values = pal) +
  xlab("MAP (mm)") +
  ylab("Aridity Index") +
  labs(fill = "Site") +
  theme(axis.text.y=element_text(size=11))

#ggsave("figures/final_figs/supp_figs/FigS4_MAPvsaridity.png", width = 10, height = 8, unit = "cm")

# Figure S12 ####
sfct = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=soil.field.capacity, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 2) +
  geom_point(aes(fill = site), colour = "black", size = 3, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Field Capacity (%)") +
  ylab(" ") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  labs(fill = "Site") +
  coord_cartesian(ylim = c(-0.8, 1.8))  +
  theme(axis.text.x=element_text(size=11)) +
  theme(axis.text.y=element_text(size=11),
        axis.title=element_text(size=13)) +
  annotate("text", x = 30, y=1.8, label = "Temporal Rarity", size = 4.5) +
  annotate("text", x = 19, y=1.42, label = "R^2: 0.621", size = 3.5, parse = TRUE) +
  annotate("text", x = 23, y=1.6, label = "Slope: 0.19 [0.02, 0.36]", size = 3.5, parse = FALSE) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 6))

sfcs = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=soil.field.capacity, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 2) +
  geom_point(aes(fill = site), colour = "black", size = 3, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Field Capacity (%)") +
  ylab("Slope") +
  geom_smooth(method = "lm", alpha = 0.15, color = "black") +
  labs(fill = "Site") +
  coord_cartesian(ylim = c(-0.8, 1.8))  +
  theme(axis.text.x=element_text(size=11)) +
  theme(axis.text.y=element_text(size=11),
        axis.title=element_text(size=13)) +
  annotate("text", x = 30, y=1.8, label = "Spatial Rarity", size = 4.5) +
  annotate("text", x = 19, y=1.42, label = "R^2: 0.736", size = 3.5, parse = TRUE) +
  annotate("text", x = 23, y=1.6, label = "Slope: 0.41 [0.11, 0.71]", size = 3.5, parse = FALSE) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 6))

ggarrange(sfcs, sfct, labels = "auto", common.legend = T, legend = "bottom")

ggsave("figures/final_figs/supp_figs/FigS12_soil_field_capacity.png", width = 15, height = 9, unit = "cm")

