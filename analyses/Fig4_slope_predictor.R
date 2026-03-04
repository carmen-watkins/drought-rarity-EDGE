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
## spatial rarity vs. dominance
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

## spatial rarity vs. temperature
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
  #annotate("text", x = 9, y=2.6, label = "p: 0.017", size = 3.5, parse = TRUE) +
  annotate("text", x = 9, y=2.6, label = "R^2: 0.743", size = 3.5, parse = TRUE)

## spatial rarity vs. MAP
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
## temporal rarity vs. dominance
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
  #annotate("text", x = 0.4, y=2.6, label = "p: 0.086", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=2.6, label = "R^2: 0.452", size = 3.5, parse = TRUE)

## temporal rarity vs. temperature
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
  annotate("text", x = 9, y=2.6, label = "R^2: 0.713", size = 3.5, parse = TRUE)

## temporal rarity vs. MAP
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

## combine panels & plot
ggarrange(pP1, pPt,
          pT1, pTt,
          pD1, pDt,
             
          ncol = 2, nrow = 3, common.legend = T, legend = "bottom", 
          labels = c("a", "b", "c", "d", "e", "f"))

## save
##ggsave("figures/review_figs/Fig4_site_slopes_predictors_drought_both_rarity.tiff", 
     #width = 15, height = 20, units = "cm")

ggsave("figures/review_figs/Fig4_site_slopes_predictors_drought_both_rarity.png",
       width = 15, height = 20, units = "cm")

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

ggsave("figures/review_figs/FigSXXX_soil_field_capacity.tiff", width = 15, height = 10, unit = "cm")

# Old Fig: Coeff plots ####
### temporal, drought
temp_slope = tmp_mods %>%
  filter(period == "Drought", term == "temporal_rarity") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = "") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13))

temp_int = tmp_mods %>%
  filter(period == "Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Drought",
       y = "") +
  labs(fill = "Site")  +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))

## spatial rarity, drought
spat_slope = sp_mods %>%
  filter(period == "Drought", term == "spatial_rarity") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = "Slope") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13))


spat_int = sp_mods %>%
  filter(period == "Drought", term == "(Intercept)") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Drought",
       y = "Intercept") +
  labs(fill = "Site") +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))

### temporal, post-drought
temp_slopep = tmp_mods %>%
  filter(period == "Post-Drought", term == "temporal_rarity") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = "") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13))

temp_intp = tmp_mods %>%
  filter(period == "Post-Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Post-Drought",
       y = "") +
  labs(fill = "Site")  +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))

## spatial rarity, post-drought
spat_slopep = sp_mods %>%
  filter(period == "Post-Drought", term == "spatial_rarity") %>%
  
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "",
       y = "") +
  labs(fill = "Site") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  coord_cartesian(xlim = c(-0.8, 2.1)) +
  theme(text = element_text(size = 13))

spat_intp = sp_mods %>%
  filter(period == "Post-Drought", term == "(Intercept)") %>%
  ggplot(aes(x = estimate, y = site)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(aes(fill = site), colour = "black", size = 3.5, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Post-Drought",
       y = " ") +
  labs(fill = "Site") +
  coord_cartesian(xlim = c(-1, 0.4)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(text = element_text(size = 13))

### plot ####
ggarrange(spat_slope, spat_slopep, temp_slope, temp_slopep,
          spat_int, spat_intp, temp_int, temp_intp,
          ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom", 
          labels = "AUTO")
#ggsave("figures/review_figs/site_slopes_intercepts_drought.tiff", 
 #      width = 8.5, height = 5.5)


# Old Fig (post-drought) ####
## spatial ####
pD2 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab(" ") +
  ylab(" ") +
  labs(fill = "Site")  +
  #geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))  +
  annotate("text", x = 0.4, y=2.6, label = "p: 0.705", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=2.3, label = "R^2: -0.200", size = 3.5, parse = TRUE)

pT2 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab(" ") +
  ylab(" ") +
  labs(fill = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black", linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))  +
  annotate("text", x = 9, y=2.6, label = "p: 0.075", size = 3.5, parse = TRUE) +
  annotate("text", x = 9, y=2.3, label = "R^2: 0.486", size = 3.5, parse = TRUE)

pP2 = spmods_pred %>%
  filter(term == "spatial_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab(" ") +
  ylab("Spatial Rarity Slope") +
  labs(fill = "Site") +
  #geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13)) +
  coord_cartesian(ylim = c(-1, 2.8))   +
  annotate("text", x = 375, y=2.6, label = "p: 0.402", size = 3.5, parse = TRUE) +
  annotate("text", x = 375, y=2.3, label = "R^2: -0.025", size = 3.5, parse = TRUE)

## temporal ####
pDt2 = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Post-Drought") %>%
  ggplot(aes(x=BP.dom.site, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.009) +
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  #facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Site-Level Dominance") +
  ylab(" ") +
  labs(color = "Site")  +
  #geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))  +
  annotate("text", x = 0.4, y=2.6, label = "p: 0.848", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=2.3, label = "R^2: -0.237", size = 3.5, parse = TRUE)

pTt2 = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Post-Drought") %>%
  
  ggplot(aes(x=MAT.C, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  #facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Temperature") +
  ylab("") +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.1, color = "black") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))  +
  annotate("text", x = 9, y=2.6, label = "p: 0.043", size = 3.5, parse = TRUE) +
  annotate("text", x = 9, y=2.3, label = "R^2: 0.601", size = 3.5, parse = TRUE)

pPt2 = tmpmods_pred %>%
  filter(term == "temporal_rarity", period == "Drought") %>%
  ggplot(aes(x=MAP.mm, y=estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 20) +
  
  geom_point(aes(fill = site), colour = "black", size = 4, pch = 21) +
  scale_fill_manual(values = pal) +
  #facet_wrap(~period, ncol = 1, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  xlab("Mean Annual Precipitation") +
  ylab("Temporal Rarity Slope") +
  labs(color = "Site") +
  #geom_smooth(method = "lm", alpha = 0.1, color = "#7d7f7c", linetype = "dashed") +
  coord_cartesian(ylim = c(-1, 2.8))  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))  +
  annotate("text", x = 375, y=2.6, label = "p: 0.187", size = 3.5, parse = TRUE) +
  annotate("text", x = 375, y=2.3, label = "R^2: 0.234", size = 3.5, parse = TRUE)

## make fig ####
ggarrange(pP2, pT2, pD2, 
          pPt2, pTt2, pDt2, 
          ncol = 3, nrow = 2, common.legend = T, legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"))

#ggsave("figures/review_figs/FigS11_site_slopes_predictors_postdrought_both_rarity.tiff", 
 #      width = 8.5, height = 6.15)

