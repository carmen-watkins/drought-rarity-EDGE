# Header ####
## Script name: Annual Perennial Models
##
## Purpose of script: Run mixed effects models incorporating duration in addition to rarity.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
library(performance)
library(parameters)
library(tidyverse)
library(car)
library(lmerTest)

library(jtools)
library(xtable)

## clean up data 
edge3 = edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial"))

# Mixed Effect Models ####
## During Drought ####
ms = lmer(resp.ratio.site_D4 ~ spatial_rarity*Duration + (1|site), data = edge3)
summary(ms)
check_model(ms)

Anova(ms, type = 3, test.statistic = "F")

ms2 = lmer(resp.ratio.site_D4 ~ spatial_rarity+Duration + (1|site), data = edge3)

anova(ms, ms2)

## Post-Drought ####
msp = lmer(resp.ratio.site_PDfull ~ spatial_rarity*Duration + (1|site), data = edge3)
summary(msp)
check_model(msp)

msp2 = lmer(resp.ratio.site_PDfull ~ spatial_rarity+Duration + (1|site), data = edge3)
summary(msp2)
check_model(msp2)

anova(msp, msp2)
## ah, no signif diff b/w models

Anova(msp2, type = 2, test.statistic = "F")

## Create Tables ####
ms_tab = as.data.frame(Anova(ms, type = 3, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Spatial")

msp_tab = as.data.frame(Anova(msp, type = 3, test.statistic = "F")) %>%
  mutate(period = "Drought",
         rarity = "Spatial")

duration_anova_df = rbind(ms_tab, msp_tab) %>%
  rownames_to_column(var = "type") %>%
  select(period, rarity, type, `F`, Df, Df.res, `Pr(>F)` )

write.csv(duration_anova_df, "tables/mixed_models_duration_spatial.csv")


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


write.csv(duration_tab, "tables/duration_site_level_lms.csv", row.names = F)








knzDd = lm(resp.ratio.site_D4 ~ spatial_rarity*Duration, data = edge3[edge3$site == "KNZ",])
summary(knzDd)

hysDd = lm(resp.ratio.site_D4 ~ spatial_rarity+Duration, data = edge3[edge3$site == "HYS",])
summary(hysDd)

chyDd = lm(resp.ratio.site_D4 ~ spatial_rarity*Duration, data = edge_RR3[edge_RR3$site == "CHY",])
summary(chyDd)

sgsDd = lm(resp.ratio.site_D4 ~ spatial_rarity+Duration, data = edge_RR3[edge_RR3$site == "SGS",])
summary(sgsDd)

sblDd = lm(resp.ratio.site_D4 ~ spatial_rarity+Duration, data = edge_RR3[edge_RR3$site == "SBL",])
summary(sblDd)

sbkDd = lm(resp.ratio.site_D4 ~ spatial_rarity+Duration, data = edge_RR3[edge_RR3$site == "SBK",])
summary(sbkDd)

sbkd = lm(resp.ratio.site_D4 ~ spatial_rarity, data = edge_RR3[edge_RR3$site == "SBK",])
summary(sbkd)
