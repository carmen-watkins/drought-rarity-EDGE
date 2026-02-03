# Header ####
## Script name: Annual Perennial Models
##
## Purpose of script: Run mixed effects models incorporating duration in addition 
## to rarity.
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
library(jtools)
library(xtable)
library(lme4)
library(MuMIn)
library(effectsize)
library(afex)
library(emmeans)

## read in data
source("analyses/supplementary_analyses/annual_perennial_figures_FigS1_S8.R")

## clean up data 
edge3 = edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial"))

# Mixed Effect Models ####
## During Drought ####
### spatial ####
ms = lmer(resp.ratio.site_D4 ~ spatial_rarity*Duration + (1|site), data = edge3)
summary(ms)
check_model(ms)

Ams2 = Anova(ms, type = 3, test.statistic = "F")
Ams2

#ms2 = lmer(resp.ratio.site_D4 ~ spatial_rarity+Duration + (1|site), data = edge3)

#anova(ms, ms2)
## ms (model with the interaction) is the significantly better model.
## I think here, we want to test for the interaction either way, as we are testing whether there is a diff drought-rarity relationship based on a species' life history

r.squaredGLMM(ms)
#eta_squared(Ams2)

### temporal #### 
mt = lmer(resp.ratio.site_D4 ~ temporal_rarity*Duration + (1|site), data = edge3)
summary(mt)

r.squaredGLMM(mt)
Anova(mt, type = 3, test.statistic = "F")

## Post-Drought ####
### spatial ####
msp = lmer(resp.ratio.site_PDfull ~ spatial_rarity*Duration + (1|site), data = edge3)
summary(msp)
check_model(msp)

#msp2 = lmer(resp.ratio.site_PDfull ~ spatial_rarity+Duration + (1|site), data = edge3)
#summary(msp2)
#check_model(msp2)

#anova(msp, msp2)
## ah, no signif diff b/w models

#Anova(msp2, type = 2, test.statistic = "F")
r.squaredGLMM(msp)

### temporal ####
mtp = lmer(resp.ratio.site_PDfull ~ temporal_rarity*Duration + (1|site), data = edge3)
summary(mtp)

r.squaredGLMM(mtp)

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
  mutate_if(is.numeric, round, digits = 3)

write.csv(duration_anova_df, "tables/review_tabs/annual_perenn_anova_TabS3.csv", row.names = F)

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
  
  mutate(signif = ifelse(`Pr(>|t|)` < 0.001, "***", 
                         ifelse(`Pr(>|t|)` < 0.01 & `Pr(>|t|)` > 0.001, "**",
                                ifelse(`Pr(>|t|)` > 0.01 & `Pr(>|t|)` < 0.05, "*", 
                                       ifelse(`Pr(>|t|)` < 0.1 & `Pr(>|t|)` > 0.05, 
                                              ".", " "))))) %>%
  # mutate_if(is.numeric, round, digits = 2) %>%
  mutate(across(where(is.numeric) & !`Pr(>|t|)`, ~round(.x, 2))) %>%
  mutate(`Pr(>|t|)` = round(`Pr(>|t|)`, digits = 3))

#xtable(coeff_df)
write.csv(coeff_df, "tables/review_tabs/annual_perenn_mixed_mod_coeff_TabS3.csv", row.names = F)





# Site level models ####
sites = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")

duration_mod_df = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge3[edge3$site == s,] %>% 
    lm(resp.ratio.site_D4 ~ spatial_rarity*Duration, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Drought") %>%
    
  
  ## append
  duration_mod_df = rbind(duration_mod_df, tmp) %>%
    filter(!is.na(term))
  
}

duration_mod_df_pd = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA,  conf.low = NA, conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  
  ## select site
  s = sites[i]
  
  ## run the model
  tmp = edge3[edge3$site == s,] %>% 
    lm(resp.ratio.site_PDfull ~ spatial_rarity*Duration, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Post-Drought")
    
    
    ## append
    duration_mod_df_pd = rbind(duration_mod_df_pd, tmp) %>%
    filter(!is.na(term))
  
}

duration_tab = rbind(duration_mod_df, duration_mod_df_pd) %>%
  select(period, site, term, estimate, conf.low, conf.high, std.error, statistic, p.value) %>%
  mutate(signif = ifelse(p.value < 0.001, "***", 
                         ifelse(p.value < 0.01 & p.value > 0.001, "**",
                                ifelse(p.value > 0.01 & p.value < 0.05, "*", 
                                       ifelse(p.value < 0.1 & p.value > 0.05, ".", " ")))))


#write.csv(duration_tab, "tables/duration_site_level_lms.csv", row.names = F)
