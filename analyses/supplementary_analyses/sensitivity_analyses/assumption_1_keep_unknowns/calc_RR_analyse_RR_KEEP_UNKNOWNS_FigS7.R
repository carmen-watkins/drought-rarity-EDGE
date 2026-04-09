# Header #### 
## Script name: Calc RR, Analyse RR, KEEP UNKNOWNS 
##
## Purpose of script: Calculate the Response Ratio from data with unknown species
## kept in. Create response ratio figures and run response ratio models to compare
## to outputs from main text results.
##
## Author: Carmen Watkins
##

# Set up ####
## load packages
library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)
library(ggpubr)

## load data
source("analyses/supplementary_analyses/sensitivity_analyses/assumption_1_keep_unknowns/calc_rank_persistence_KEEP_UNKNOWNS.R")

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

# Calc Resp Ratio ####
## Drought ####
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

RR.tog = rbind(drought.SE.RII, recov.SE.RII) %>%
  select(site, species, resp.ratio.site, SE.RII, treatment.period) %>%
  pivot_wider(names_from = treatment.period, values_from = c(resp.ratio.site, SE.RII))#

## merge with rank and persistence values for each species
edge_RR = left_join(RR.tog, rank_persist, by = c("site", "species"))

## Clean up ####
rm(edge_all, drought.SE.RII, recov.SE.RII, RR.tog, rank_persist)

# Plot####
edge_RR$site = factor(edge_RR$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## Fig S7 ####
## test out main pattern
SR_drought = ggplot(edge_RR, aes(x= spatial_rarity, y=resp.ratio.site_D, color = site)) +
  geom_point(alpha = 0.9, size = 1) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.1, linewidth = 1) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13)) +
  coord_cartesian(ylim = c(-1,1.38)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.15", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.21", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.91 [0.71, 1.11]", size = 3.5, parse = FALSE)

SR_postdrought = ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PD, color = site)) +
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
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.15", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.18", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.89 [0.70, 1.07]", size = 3.5, parse = FALSE)

TR_drought = ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D, color = site)) +
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
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.13", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.16", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.66 [0.50, 0.82]", size = 3.5, parse = FALSE)

TR_postdrought <- ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_PD, color = site)) +
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
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.10", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.12", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "Slope: 0.60 [0.44, 0.76]", size = 3.5, parse = FALSE)

ggarrange(SR_drought, TR_drought, SR_postdrought, TR_postdrought,
          labels = "auto", common.legend = T, legend = "bottom", ncol = 2, nrow=2)

## ggsave("figures/review_figs/FigS7_RR_v_rarity_keep_unknowns.tiff", width = 18, height = 16, units = "cm")

ggsave("figures/final_figs/supp_figs/FigS7_RR_v_rarity_keep_unknowns.png",
       width = 18, height = 16, units = "cm")

# Check Models ####
## drought, spatial ####
mmsd = lmer(resp.ratio.site_D ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsd)
summary(mmsd) ## supp table
Anova(mmsd, type = 2, test.statistic = "F")

r.squaredGLMM(mmsd)
confint(mmsd)

## drought, temporal ####
mmtd = lmer(resp.ratio.site_D ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtd)
summary(mmtd) ## supp table
Anova(mmtd, type = 2, test.statistic = "F") ## main table
r.squaredGLMM(mmtd)
confint(mmtd)

## post-drought spatial ####
mmsp = lmer(resp.ratio.site_PD ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsp)
summary(mmsp) ## supp table
Anova(mmsp, type = 2, test.statistic = "F") ## main table
confint(mmsp)

r.squaredGLMM(mmsp)

## post-drought temporal ####
mmtp = lmer(resp.ratio.site_PD ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtp)
summary(mmtp) ## supp table
Anova(mmtp, type = 3, test.statistic = "F") ## main table
confint(mmtp)

r.squaredGLMM(mmtp)

# Create Tables ####
## Anova ####
### decided to use type II Anovas - for when data is unbalanced and DON'T want to consider interactions
mmsd_tab = as.data.frame(Anova(mmsd, type = 2, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Spatial") %>%
  mutate_if(is.numeric, round, digits = 3)

mmtd_tab = as.data.frame(Anova(mmtd, type = 2, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Temporal") %>%
  mutate_if(is.numeric, round, digits = 3)

mmsp_tab = as.data.frame(Anova(mmsp, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Spatial") %>%
  mutate_if(is.numeric, round, digits = 3)

mmtp_tab = as.data.frame(Anova(mmtp, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal") %>%
  mutate_if(is.numeric, round, digits = 3)

anova_df = rbind(mmsd_tab, mmtd_tab, mmsp_tab, mmtp_tab) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)` )
## write.csv(anova_df, "tables/review_tabs/TabS4_mixed_mod_anova_table_SA_Keep_Unknowns.csv")


## Coeff ####
mmsd_coeff = as.data.frame(summary(mmsd)$coefficients) %>% 
  mutate(period = "Drought",
         rarity = "Spatial")

mmtd_coeff = as.data.frame(summary(mmtd)$coefficients) %>% 
  mutate(period = "Drought",
         rarity = "Temporal")

mmsp_coeff = as.data.frame(summary(mmsp)$coefficients)%>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

mmtp_coeff = as.data.frame(summary(mmtp)$coefficients) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

coeff_df = rbind(mmsd_coeff, mmtd_coeff, mmsp_coeff, mmtp_coeff) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, Estimate, `Std. Error`, df, `t value`, `Pr(>|t|)`) %>%
  mutate_if(is.numeric, round, digits = 3)

## write.csv(coeff_df, "tables/review_tabs/TabS4_mixed_mod_coeff_table_SA_Keep_Unknowns.csv", row.names = F)

