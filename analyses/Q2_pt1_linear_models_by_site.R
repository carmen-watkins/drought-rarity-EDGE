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
#library(parameters)
library(tidyverse)
library(car)
library(jtools)
library(xtable)

source("analyses/calc_response_ratio.R") 
source("data-prep/prep_model_predictors.R")
#source("analyses/color_palettes.R")

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

# Model ####
## spatial, drought ####
sites = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")

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

## combine ####
sp_mods = rbind(mod_df, mod_dfp) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

tmp_mods = rbind(modt_df, modt_dfp) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

# Create Table ####
sp_mods_tab = sp_mods %>%
  select(period, site, term, estimate, std.error, statistic, p.value) %>%
  mutate(signif = ifelse(p.value < 0.001, "***", 
                         ifelse(p.value < 0.01 & p.value > 0.001, "**",
                                ifelse(p.value > 0.01 & p.value < 0.05, "*", 
                                       ifelse(p.value < 0.1 & p.value > 0.05, 
                                              ".", " ")))))

#write.csv(sp_mods_tab, "tables/site_model_output_spatial.csv")

tmp_mods_tab = tmp_mods %>%
  select(period, site, term, estimate, std.error, statistic, p.value) %>%
  mutate(signif = ifelse(p.value < 0.001, "***", 
                         ifelse(p.value < 0.01 & p.value > 0.001, "**",
                                ifelse(p.value > 0.01 & p.value < 0.05, "*", 
                                       ifelse(p.value < 0.1 & p.value > 0.05, 
                                              ".", " ")))))

#write.csv(tmp_mods_tab, "tables/site_model_output_temporal.csv")

# Clean Env ####
rm(chy_unks, hys_unks, knz_temp, knz_unks, sbk_unks, sgs_unks, virid_sp, 
   tmp_mods_tab, sp_mods_tab, mod_df, mod_dfp, modt_df, modt_dfp, tmp)
