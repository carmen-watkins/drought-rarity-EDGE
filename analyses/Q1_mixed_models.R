# Header ####
## Script name: Q1 Mixed Models
##
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

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

# Model ####
## model as way of estimating the overall effect of rarity on response ratio
## during drought and post-drought for spatial and temporal rarity. good that 
## it still accounts for effect of site.

## drought, spatial ####
mmsd = lmer(resp.ratio.site_D4 ~ spatial_rarity + (1|site), data = edge_RR_cats)

#check_model(mmsd)
summary(mmsd) ## supp table
Atable = Anova(mmsd, type = 2, test.statistic = "F") ## main table
Atable
confint(mmsd)

## effect sizes: 
0.87491 / sqrt(0.03003 + 0.37390)

## diff b/w means divided by sqrt of var intercept + var slope + var residual
## note, it seems like if you have multiple random effects, you just add 
## them all into this square root term.

r.squaredGLMM(mmsd)
eta_squared(Atable)

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
### decided to use type II Anovas - for when data is unbalanced and DON'T want 
## to consider interactions
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
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)`, signif) 

write.csv(anova_df, "tables/review_tabs/Q1_mixed_mod_anova_TabS9.csv", 
          row.names = F)

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
  select(period, rarity, type, Estimate, `Std. Error`, df, `t value`, 
         `Pr(>|t|)`) %>%
 # mutate_if(is.numeric, round, digits = 2) %>%
  mutate(across(where(is.numeric) & !`Pr(>|t|)`, ~round(.x, 2))) %>%
  mutate(`Pr(>|t|)` = round(`Pr(>|t|)`, digits = 3))

write.csv(coeff_df, "tables/review_tabs/Q1_mixed_mod_coeff_Tab1.csv", 
          row.names = F)
