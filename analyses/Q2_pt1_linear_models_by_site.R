# Header ####
## Script name: Q2 Linear Models by Site

## Purpose of script: Run linear models to test the effect of rarity on response 
## ratio separately at each site
##
## Author: Carmen Watkins
##

# Set up ####
## load packages
library(broom)
library(performance)
library(tidyverse)
library(car)

source("analyses/calc_response_ratio.R") 
source("data-prep/prep_model_predictors.R")

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

# Model ####
## spatial, drought ####
sites = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")

## create an empty dataframe for outputs
mod_df = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, 
                    p.value = NA,  conf.low = NA, conf.high = NA, site = NA, 
                    period = NA)

for(i in 1:length(sites)) {
  ## select site
  s = sites[i]
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_D4 ~ spatial_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Drought")
  ## append
  mod_df = rbind(mod_df, tmp) %>%
    filter(!is.na(term))
  
}

## spatial, post-drought ####
mod_dfp = data.frame(term = NA, estimate = NA, std.error = NA, 
                     statistic = NA, p.value = NA,  conf.low = NA, 
                     conf.high = NA, site = NA, period = NA)

for(i in 1:length(sites)) {
  ## select site
  s = sites[i]
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_PDfull ~ spatial_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Post-Drought")
  ## append
  mod_dfp = rbind(mod_dfp, tmp) %>%
    filter(!is.na(term))
  
}

## temporal, drought ####
modt_df = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, 
                     p.value = NA,  conf.low = NA, conf.high = NA, site = NA, 
                     period = NA)

for(i in 1:length(sites)) {
  ## select site
  s = sites[i]
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_D4 ~ temporal_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Drought")
  ## append
  modt_df = rbind(modt_df, tmp) %>%
    filter(!is.na(term))
  
}

## temporal, post-drought ####
modt_dfp = data.frame(term = NA, estimate = NA, std.error = NA, statistic = NA, 
                      p.value = NA,  conf.low = NA, conf.high = NA, site = NA, 
                      period = NA)

for(i in 1:length(sites)) {
  ## select site
  s = sites[i]
  ## run the model
  tmp = edge_RR[edge_RR$site == s,] %>% 
    lm(resp.ratio.site_PDfull ~ temporal_rarity, data = .) %>% 
    tidy(conf.int = TRUE) %>%
    mutate(site = s, 
           period = "Post-Drought")
  ## append
  modt_dfp = rbind(modt_dfp, tmp) %>%
    filter(!is.na(term))
}

## combine dfs ####
sp_mods = rbind(mod_df, mod_dfp) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

tmp_mods = rbind(modt_df, modt_dfp) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

# Create Table ####
mods_tab = rbind(sp_mods, tmp_mods) %>%
  select(period, site, term, estimate, conf.low, conf.high, std.error, 
         statistic, p.value) %>%
  mutate(across(where(is.numeric) & !p.value, ~round(.x, 2))) %>%
  mutate(p.value = round(p.value, 10))

#write.csv(mods_tab, "tables/final_tables/TabS8_site_model_output_all.csv",
 #         row.names = F)

# Clean Env ####
rm(chy_unks, hys_unks, knz_temp, knz_unks, sbk_unks, sgs_unks, virid_sp,
   mod_df, mod_dfp, modt_df, modt_dfp, tmp)

# Get F and R2 values ####
## spatial ####
sgs = edge_RR[edge_RR$site == "SGS",] %>% 
  lm(resp.ratio.site_PDfull ~ spatial_rarity, data = .)

summary(sgs)

sbk = edge_RR[edge_RR$site == "SBK",] %>% 
  lm(resp.ratio.site_D4 ~ temporal_rarity, data = .)

summary(sbk)
