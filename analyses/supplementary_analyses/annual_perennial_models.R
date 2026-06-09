# Header ####
## Script name: Annual Perennial Models
##
## Purpose of script: Run mixed effects models incorporating duration in  
## addition to rarity.
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
library(lme4)
library(MuMIn)
library(emmeans)

## read in data
source("analyses/supplementary_analyses/annual_perennial_figures_FigS5.R")

## clean up data 
edge3 = edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial"))

# Mixed Effect Models ####
## Drought ####
### spatial ####
ms = lmer(resp.ratio.site_D4 ~ spatial_rarity*Duration + (1|site), 
          data = edge3)
summary(ms)
#check_model(ms)

Anova(ms, type = 3, test.statistic = "F")

#ms2 = lmer(resp.ratio.site_D4 ~ spatial_rarity+Duration + (1|site), 
#data = edge3)

#anova(ms, ms2)
##' ms (model with the interaction) is the significantly better model.
##' I think here, we want to test for the interaction either way, 
##' as we are testing whether there is a diff drought-rarity relationship 
##' based on a species' life history

r.squaredGLMM(ms)
emtrends(ms, specs = "Duration", var = "spatial_rarity")
pairs(emtrends(ms, specs = "Duration", var = "spatial_rarity"))
## slopes are significantly different

### temporal #### 
mt = lmer(resp.ratio.site_D4 ~ temporal_rarity*Duration + (1|site), 
          data = edge3)
summary(mt)

r.squaredGLMM(mt)
Anova(mt, type = 3, test.statistic = "F")

emtrends(mt, specs = "Duration", var = "temporal_rarity")
pairs(emtrends(mt, specs = "Duration", var = "temporal_rarity"))

## Post-Drought ####
### spatial ####
msp = lmer(resp.ratio.site_PDfull ~ spatial_rarity*Duration + (1|site), 
           data = edge3)
summary(msp)
#check_model(msp)

#msp2 = lmer(resp.ratio.site_PDfull ~ spatial_rarity+Duration + (1|site), 
#data = edge3)
#summary(msp2)
#check_model(msp2)

#anova(msp, msp2)
## ah, no signif diff b/w models

#Anova(msp2, type = 2, test.statistic = "F")
r.squaredGLMM(msp)
Amsp = Anova(msp, type = 3, test.statistic = "F")
Amsp

emtrends(msp, specs = "Duration", var = "spatial_rarity")

pairs(emtrends(msp, specs = "Duration", var = "spatial_rarity"))


### temporal ####
mtp = lmer(resp.ratio.site_PDfull ~ temporal_rarity*Duration + (1|site), 
           data = edge3)
summary(mtp)

r.squaredGLMM(mtp)
Anova(mtp, type = 3, test.statistic = "F")

emtrends(mtp, specs = "Duration", var = "temporal_rarity")
pairs(emtrends(mtp, specs = "Duration", var = "temporal_rarity"))

## Create Tables ####
### Anova table ####
ms_tab = as.data.frame(Anova(ms, type = 3, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Spatial")

mt_tab = as.data.frame(Anova(mt, type = 3, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Temporal")

msp_tab = as.data.frame(Anova(msp, type = 3, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

mtp_tab = as.data.frame(Anova(mtp, type = 3, test.statistic = "F")) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

duration_anova_df = rbind(ms_tab, mt_tab, msp_tab, mtp_tab) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)` ) %>%
  mutate(across(where(is.numeric) & !`Pr(>F)`, ~round(.x, 2))) %>%
  mutate(`Pr(>F)` = round(`Pr(>F)`, digits = 3))

## write.csv(duration_anova_df, "tables/review_tabs/annual_perenn_anova_TabS3.csv", 
   ##       row.names = F)

### Coeff Table ####
ms_coeff = as.data.frame(summary(ms)$coefficients) %>% 
  mutate(period = "Drought",
         rarity = "Spatial")

mt_coeff = as.data.frame(summary(mt)$coefficients) %>% 
  mutate(period = "Drought",
         rarity = "Temporal")

msp_coeff = as.data.frame(summary(msp)$coefficients)%>%
  mutate(period = "Post-Drought",
         rarity = "Spatial")

mtp_coeff = as.data.frame(summary(mtp)$coefficients) %>%
  mutate(period = "Post-Drought",
         rarity = "Temporal")

coeff_df = rbind(ms_coeff, mt_coeff, msp_coeff, mtp_coeff) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, Estimate, `Std. Error`, df, `t value`, `Pr(>|t|)`) %>%
  mutate(across(where(is.numeric) & !`Pr(>|t|)`, ~round(.x, 2))) %>%
  mutate(`Pr(>|t|)` = round(`Pr(>|t|)`, digits = 3))

#xtable(coeff_df)
#write.csv(coeff_df, "tables/review_tabs/annual_perenn_mixed_mod_coeff_TabS3.csv", 
 #         row.names = F)
