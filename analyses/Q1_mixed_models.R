# Header ####
## Script name: Q1 Mixed Models

## Purpose of script: Run linear mixed effects models to test the overall 
## effect of rarity on response ratio
##
## Author: Carmen Watkins
##

# Set up ####
library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)
library(jtools)
library(xtable)
library(MuMIn)

library(effectsize)
library(afex)
library(emmeans)

source("analyses/calc_response_ratio.R") 
#source("analyses/color_palettes.R")

## read in FG data
FG = read.csv(here::here("data","edge_species_info_CP_BA.csv"))

#Join functional group data to species response ratio data
edge_RR2 = edge_RR %>%
  left_join(FG, by = "species") %>%
  mutate(FunctionalGroup = ifelse(species %in% c("Astragalus_sp", "Eriogonum_sp",
                                                 "Euphorbia_sp", "Oenothera_sp",
                                                 "Asclepias_syriaca", "Cirsium_sp",
                                                 "Astragalus_Oxytropis_sp"), 
                                  "forb", 
                                  ifelse(species %in% c("Sporobolus_sp"), 
                                         "grass", FunctionalGroup))) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"),
         Duration = ifelse(site == "KNZ" & species == "Asclepias_syriaca", 
                           "perennial", Duration),
         Duration = ifelse(species %in% c("Astragalus_drummondii", 
                                          "Astragalus_laxmanii", 
                                          "Astragalus_Oxytropis_sp", 
                                          "Astragalus_shortianus", 
                                          "Astragalus_sp", 
                                          "Astragulus_crassicarpus"), 
                           "perennial", Duration),
         Duration = ifelse(species %in% c("Euphorbia_exstipulata", 
                                          "Euphorbia_sp", "Euphorbia_sp."), 
                           "annual", Duration), 
         Duration = ifelse(species %in% c("Sporobolus_asper", 
                                          "Sporobolus_cryptandrus", 
                                          "Sporobolus_heterolepis", 
                                          "Sporobolus_sp", "Sporobolus_sp."), 
                           "perennial", Duration), 
         Duration = ifelse(is.na(Duration) | Duration == "unk", 
                           "unknown", Duration))

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")
#wes_palette("Royal3")

# Model ####
## model as way of estimating the overall effect of rarity on response ratio
## during drought and post-drought for spatial and temporal rarity. good that 
## it still accounts for effect of site.

## drought, spatial ####
mmsd = lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsd)
summary(mmsd) ## supp table
Atable = Anova(mmsd, type = 2, test.statistic = "F") ## main table
Atable
#nice(Atable)

xint = -(-0.30282)/0.86135
## VarCorr could be helpful for extracting model output

coef(mmsd)$site[,"(Intercept)"]
confint(mmsd)

## effect sizes: 
## 
0.87491 / sqrt(0.03003 + 0.37390)

## diff b/w means divided by sqrt of var intercept + var slope + var residual

## note, it seems like if you have multiple random effects, you just add them all into this square root term.

r.squaredGLMM(mmsd)
eta_squared(Atable)

#emmeans(mmsd, specs = "spatial_rarity", at = list(x = c(0, 0.25, 0.5, 0.75, 1)))

#emtrends(mmsd, specs = "spatial_rarity", var = "site")

## drought, temporal ####
mmtd = lmer(resp.ratio.site_D4 ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtd)
summary(mmtd) ## supp table
Atable2 = Anova(mmtd, type = 2, test.statistic = "F") ## main table
confint(mmtd)
Atable2

## calc effect sizes
r.squaredGLMM(mmtd)
eta_squared(Atable2)

## post-drought spatial ####
mmsp = lmer(resp.ratio.site_PDfull ~ spatial_rarity + (1|site), data = edge_RR)

#check_model(mmsp)
summary(mmsp) ## supp table
Atable3 = Anova(mmsp, type = 2, test.statistic = "F") ## main table
confint(mmsp)
Atable3

## calc effect sizes
r.squaredGLMM(mmsp)
eta_squared(Atable3)

## post-drought temporal ####
mmtp = lmer(resp.ratio.site_PDfull ~ temporal_rarity + (1|site), data = edge_RR)

#check_model(mmtp)
summary(mmtp) ## supp table
Atable4 = Anova(mmtp, type = 2, test.statistic = "F") ## main table
confint(mmtp)

Atable4

## calc effect sizes
r.squaredGLMM(mmtp)
eta_squared(Atable4)


# Create Tables ####
## Anova ####
### decided to use type II Anovas - for when data is unbalanced and DON'T want to consider interactions
mmsd_tab = as.data.frame(Anova(mmsd, type = 2, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Spatial")

mmtd_tab = as.data.frame(Anova(mmtd, type = 2, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Temporal")

mmsp_tab = as.data.frame(Anova(mmsp, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

mmtp_tab = as.data.frame(Anova(mmtp, type = 2, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

anova_df = rbind(mmsd_tab, mmtd_tab, mmsp_tab, mmtp_tab) %>%
  rownames_to_column(var = "type") %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(signif = ifelse(`Pr(>F)` < 0.001, "***", 
                         ifelse(`Pr(>F)` < 0.01 & `Pr(>F)` > 0.001, "**",
                                ifelse(`Pr(>F)` > 0.01 & `Pr(>F)` < 0.05, "*", 
                                       ifelse(`Pr(>F)` < 0.1 & `Pr(>F)` > 0.05, 
                                              ".", " "))))) %>%
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)`, signif) 

write.csv(anova_df, "tables/review_tabs/Q1_mixed_mod_anova_TabS9.csv", row.names = F)
#xtable(anova_df)

## Coeff ####
mmsd_coeff = as.data.frame(summary(mmsd)$coefficients) %>% 
  #lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = .) %>% 
  #tidy(conf.int = TRUE) %>%
  mutate(period = "Drought",
         rarity = "Spatial")

mmtd_coeff = as.data.frame(summary(mmtd)$coefficients) %>% 
  #lmer(resp.ratio.site_D4 ~ temporal_rarity + (1|site), data = .) %>% 
  #tidy(conf.int = TRUE) %>%
  mutate(period = "Drought",
         rarity = "Temporal")

mmsp_coeff = as.data.frame(summary(mmsp)$coefficients)%>%
 # lmer(resp.ratio.site_PDfull ~ spatial_rarity + (1|site), data = .) %>% 
  #tidy(conf.int = TRUE) %>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

mmtp_coeff = as.data.frame(summary(mmtp)$coefficients) %>%
 # lmer(resp.ratio.site_PDfull ~ temporal_rarity + (1|site), data = .) %>% 
#  tidy(conf.int = TRUE) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

coeff_df = rbind(mmsd_coeff, mmtd_coeff, mmsp_coeff, mmtp_coeff) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, Estimate, `Std. Error`, df, `t value`, `Pr(>|t|)`) %>%
  
  mutate(signif = ifelse(`Pr(>|t|)` < 0.001, "***", 
                         ifelse(`Pr(>|t|)` < 0.01 & `Pr(>|t|)` > 0.001, "**",
                                ifelse(`Pr(>|t|)` > 0.01 & `Pr(>|t|)` < 0.05, "*", 
                                       ifelse(`Pr(>|t|)` < 0.1 & `Pr(>|t|)` > 0.05, 
                                              ".", " "))))) %>%
 # mutate_if(is.numeric, round, digits = 2) %>%
  mutate(across(where(is.numeric) & !`Pr(>|t|)`, ~round(.x, 2))) %>%
  mutate(`Pr(>|t|)` = round(`Pr(>|t|)`, digits = 3))

#xtable(coeff_df)
write.csv(coeff_df, "tables/review_tabs/Q1_mixed_mod_coeff_Tab1.csv", row.names = F)

# Plot Old Fig 1 version ####
p1 = effect_plot(mmsd, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Drought Response Ratio", x.label = " ", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

## too many sites & points for coloring points to be easily interpretable; can include supplementary figures to make this point
#ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PDfull)) +
 # geom_point(aes(color = site)) +
 # geom_smooth(method = "lm", color = "black") +
#  scale_color_manual(values = pal)

p2 = effect_plot(mmtd, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, y.label = " ", x.label = " ", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

p3 = effect_plot(mmsp, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Post-Drought Response Ratio", x.label = "Spatial Rarity", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

p4 = effect_plot(mmtp, pred = temporal_rarity, interval = TRUE, plot.points = TRUE, y.label = " ", x.label = "Temporal Rarity", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

ggarrange(p1, p2, p3, p4, labels = "AUTO")

## ggsave("figures/Jan2025/resp_ratio_v_rarity_mmfit.tiff", width = 7.5, height = 7)

# Old Fig 1 version: Spatial ####
pS1 = effect_plot(mmsd, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Drought Response Ratio", x.label = "Spatial Rarity", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

pS2 = effect_plot(mmsp, pred = spatial_rarity, interval = TRUE, plot.points = TRUE, y.label = "Post-Drought Response Ratio", x.label = "Spatial Rarity", 
                 colors = "#909090", 
                 line.colors = "black") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12),
        axis.title=element_text(size=13))

ggarrange(pS1, pS2, labels = "AUTO")

#ggsave("figures/Jan2025/SO_resp_ratio_v_rarity_mmfit.tiff", width = 7.5, height = 3.5)


