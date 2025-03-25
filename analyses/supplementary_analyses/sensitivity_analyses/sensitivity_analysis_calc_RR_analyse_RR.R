source("analyses/supplementary_analyses/sensitivity_analyses/sensitivity_analysis_calc_rank_persistence_keep_unknowns.R")

source("analyses/color_palettes.R")

library(ggpubr)

## set up graphics
theme_set(theme_classic())
pal <- wes_palette("Royal3")
wes_palette("Royal3")


# Resp Ratio ####
## Drought ####
### 4-year ####
drought.SE.RII <- edge_all %>%
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  
  pivot_wider(names_from = "treatment", values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  
  ungroup() %>%
  
  ## calculate block level resp ratio & SE
  mutate(resp.ratio.site = (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D), ## calc response ratio
         
         ## calc error of RII
         ## rho
         rho = (((sd.cover.sp_D^2)/num.obs_D) - ((sd.cover.sp_C^2)/num.obs_C)) / ((sd.cover.sp_D^2/num.obs_D) + (sd.cover.sp_C^2/num.obs_C)), ## calc rho as part of standard error calc
         
         ## term outside of parentheses
         outpar = ((sd.cover.sp_D^2)/num.obs_D + (sd.cover.sp_C^2)/num.obs_C) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 1 inside parentheses
         term1 = ((mean.cover.sp_D - mean.cover.sp_C)^2) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 2 inside parentheses
         term2 = (2 * rho * (mean.cover.sp_D - mean.cover.sp_C)) / (mean.cover.sp_D + mean.cover.sp_C),
         
         ## calc inside of parentheses
         inpar = 1 + term1 - term2,
         
         ## calc SE
         SE.RII = outpar * inpar,
         
         treatment.period = "D") ## add in column to differentiate from post-drought RR



## Recovery ####
recov.SE.RII <- edge_all %>%
  filter(treatment.year == "recovery") %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  
  summarise(mean.cover.sp = mean(max.cover), ## mean cover by site across years
            sd.cover.sp = sd(max.cover), ## calc sd of cover for use in error calcs
            num.obs = n()) %>% 
  
  pivot_wider(names_from = "treatment", values_from = c("mean.cover.sp", "sd.cover.sp", "num.obs")) %>% 
  
  ungroup() %>%
  
  ## calculate block level resp ratio & SE
  mutate(resp.ratio.site = (mean.cover.sp_D-mean.cover.sp_C)/(mean.cover.sp_C+mean.cover.sp_D), ## calc response ratio
         
         ## calc error of RII
         ## rho
         rho = (((sd.cover.sp_D^2)/num.obs_D) - ((sd.cover.sp_C^2)/num.obs_C)) / ((sd.cover.sp_D^2/num.obs_D) + (sd.cover.sp_C^2/num.obs_C)), ## calc rho as part of standard error calc
         
         ## term outside of parentheses
         outpar = ((sd.cover.sp_D^2)/num.obs_D + (sd.cover.sp_C^2)/num.obs_C) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 1 inside parentheses
         term1 = ((mean.cover.sp_D - mean.cover.sp_C)^2) / ((mean.cover.sp_D + mean.cover.sp_C)^2),
         
         ## term 2 inside parentheses
         term2 = (2 * rho * (mean.cover.sp_D - mean.cover.sp_C)) / (mean.cover.sp_D + mean.cover.sp_C),
         
         ## calc inside of parentheses
         inpar = 1 + term1 - term2,
         
         ## calc SE
         SE.RII = outpar * inpar,
         
         treatment.period = "PD") ## add in column to differentiate from post-drought RR

RR.tog <- rbind(drought.SE.RII, recov.SE.RII) %>%
  select(site, species, resp.ratio.site, SE.RII, treatment.period) %>%
  pivot_wider(names_from = treatment.period, values_from = c(resp.ratio.site, SE.RII))#

## merge with rank and persistence values for each species
edge_RR <- left_join(RR.tog, rank_persist, by = c("site", "species"))


# Clean up ####
rm(edge_all, drought.SE.RII, recov.SE.RII, RR.tog, edge_w_zeros, rank_persist, SEVcheck)


# Check RESults ####
edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))


## test out main pattern
SR_drought <- ggplot(edge_RR, aes(x= spatial_rarity, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1))

SR_postdrought <- ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13))

TR_drought <- ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13))

TR_postdrought <- ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_PD)) +
  geom_point(alpha = 0.9, size = 0.8, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 0.75) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13))

ggarrange(SR_drought, TR_drought, SR_postdrought, TR_postdrought,
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 2, nrow=2)

ggsave("figures/Mar2025/FigS5_RR_v_rarity_keep_unknowns.tiff", width = 6, height = 5.5)
