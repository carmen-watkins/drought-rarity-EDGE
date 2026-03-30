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
  #annotate("text", x = 0.4, y=2.6, label = "p: 0.236", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=2.6, label = "R^2: 0.158", size = 3.5, parse = TRUE)

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
  annotate("text", x = 9, y=2.2, label = "R^2: 0.743", size = 3.5, parse = TRUE)

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
  annotate("text", x = 600, y=2.7, label = "Spatial Rarity", size = 4.5) +
  annotate("text", x = 375, y=2.3, label = "R^2: -0.19", size = 3.5, parse = TRUE)

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
  annotate("text", x = 0.45, y=2.6, label = "Slope: 2.84 [-0.64, 6.33]", size = 3.5, parse = FALSE) +
  annotate("text", x = 0.4, y=2.2, label = "R^2: 0.452", size = 3.5, parse = TRUE)

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
  annotate("text", x = 9, y=2.2, label = "R^2: 0.713", size = 3.5, parse = TRUE)

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
  annotate("text", x = 600, y=2.7, label = "Temporal Rarity", size = 4.5) +
  annotate("text", x = 375, y=2.3, label = "R^2: -0.250", size = 3.5, parse = TRUE)

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
     width = 15, height = 20, units = "cm")

#ggsave("figures/review_figs/Fig4_site_slopes_predictors_drought_both_rarity.png",
 #      width = 15, height = 20, units = "cm")

# Figure S12 ####
sfct = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=soil.field.capacity, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 2) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Field Capacity (%)") +
  ylab(" ") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black") +
  labs(fill = "Site") +
  coord_cartesian(ylim = c(-0.8, 1.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  annotate("text", x = 30, y=1.7, label = "Temporal Rarity", size = 4.5) +
  annotate("text", x = 19, y=1.4, label = "R^2: 0.621", size = 3.5, parse = TRUE)


sfcs = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=soil.field.capacity, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 2) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Field Capacity (%)") +
  ylab("Slope") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black") +
  labs(fill = "Site") +
  coord_cartesian(ylim = c(-0.8, 1.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  annotate("text", x = 30, y=1.7, label = "Spatial Rarity", size = 4.5) +
  annotate("text", x = 19, y=1.4, label = "R^2: 0.736", size = 3.5, parse = TRUE)

ggarrange(sfcs, sfct, labels = "auto", common.legend = T, legend = "bottom")

ggsave("figures/review_figs/FigS12_soil_field_capacity.tiff", width = 15, height = 10, unit = "cm")

