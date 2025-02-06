# Header #### 
## Script name: 
##
## Purpose of script: Fill zeros
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
source("data-prep/clean_cover_dat_sp_names.R")

# Fill Zeros ####
## SBL ####
sbl_fill = sbl_sp %>%
  group_by(site, treatment, block, plot, subplot, year, experiment.year, treatment.year, species, spcode, kartez) %>% 
  
  ## get max cover by season
  summarise(max.cover = max(cover)) %>%
  
  ## change from E -> D for more intuitive notation
  mutate(treatment = ifelse(treatment == "E", "D", treatment)) %>% 
  ungroup() %>%
  select(-spcode, -kartez) %>%
  
  ## fill zeros 
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_gracilis:Ipomoea_costellata, names_to = "species", values_to = "max.cover") 

## SBK ####
sbk_fill = sbk_sp %>%
  group_by(site, treatment, block, plot, subplot, year, experiment.year, treatment.year, species, spcode, kartez) %>% 
  
  ## get max cover by season
  summarise(max.cover = max(cover)) %>%
  
  ## change from E -> D for more intuitive notation
  mutate(treatment = ifelse(treatment == "E", "D", treatment)) %>% 
  ungroup() %>%
  select(-spcode, -kartez) %>%
  
  ## fill zeros 
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_eriopoda:Psilostrophe_tagetina, names_to = "species", values_to = "max.cover")

## KNZ ####
## separate and fill with 0's  
knz_fill = knz_sp %>%
  select(-sp.ep, -spcode, -genus) %>%
  
  ## there are duplicate rows, very likely from lumping certain genera; within a subplot, add cover of lumped species
  group_by(year, experiment.year, treatment.year, site, block, plot, subplot,  treatment, species) %>%
  summarise(max.cover2 = sum(max.cover)) %>%
  ungroup() %>%
    
  pivot_wider(names_from = "species", values_from = "max.cover2", values_fill = 0) %>%
  pivot_longer(Achillea_millefolium:Desmodium_illinoense, names_to = "species", values_to = "max.cover") 
## 9-years * 20 plots * 4 subplots * 78 species = 56160; this checks out for one obs per species per subplot per years

## HYS ####
hys_fill = hys_sp %>%
  select(-sp.ep, -spcode, -genus) %>%
  
  ## there are duplicate rows, very likely from lumping certain genera; within a subplot, add cover of lumped species
  group_by(year, experiment.year, treatment.year, site, block, plot, subplot,  treatment, species) %>%
  summarise(max.cover2 = sum(max.cover)) %>%
  ungroup() %>%

  pivot_wider(names_from = "species", values_from = "max.cover2", values_fill = 0) %>%
  pivot_longer(Achillea_millefolium:Descurainia_pinnata, names_to = "species", values_to = "max.cover")
## 9-years * 20 plots * 4 subplots * 107 species = 77040; this checks out for one obs per species per subplot per years

## CHY ####
chy_fill = chy_sp %>%
  select(-sp.ep, -spcode, -genus) %>%
  
  ## there are duplicate rows, very likely from lumping certain genera; within a subplot, add cover of lumped species
  group_by(year, experiment.year, treatment.year, site, block, plot, subplot,  treatment, species) %>%
  summarise(max.cover2 = sum(max.cover)) %>%
  ungroup() %>%
  
  pivot_wider(names_from = "species", values_from = "max.cover2", values_fill = 0) %>%
  pivot_longer(Artemesia_frigida:Ratibida_columnifera, names_to = "species", values_to = "max.cover") 
## 9-years * 20 plots * 4 subplots * 79 species = 56880; this checks out for one obs per species per subplot per years

## SGS ####
sgs_fill = sgs_sp %>%
  select(-sp.ep, -spcode, -genus) %>%
  
  ## there are duplicate rows, very likely from lumping certain genera; within a subplot, add cover of lumped species
  group_by(year, experiment.year, treatment.year, site, block, plot, subplot,  treatment, species) %>%
  summarise(max.cover2 = sum(max.cover)) %>%
  ungroup() %>%
  
  pivot_wider(names_from = "species", values_from = "max.cover2", values_fill = 0) %>%
  pivot_longer(Bouteloua_gracilis:Picradeniopsis_oppositifolia, names_to = "species", values_to = "max.cover")
## 9-years * 20 plots * 4 subplots * 61 species = 43920; this checks out for one obs per species per subplot per years

# Merge ####
## combine zero filled sites
edge_all = rbind(knz_fill, hys_fill, chy_fill, sgs_fill, sbl_fill, sbk_fill) %>%
  mutate(pres.abs = ifelse(max.cover > 0, 1, 0),
         recov.year = ifelse((site %in% c("SBK", "SBL") & experiment.year %in% c(8,9)) | (site %in% c("KNZ", "HYS", "CHY", "SGS") & experiment.year %in% c(5,6)), "initial", 
                             ifelse((site %in% c("SBK", "SBL") & experiment.year %in% c(10,11)) | (site %in% c("KNZ", "HYS", "CHY", "SGS") & experiment.year %in% c(7,8)), "final", NA))) %>%
  
  select(year, site, treatment, block, plot, subplot, species, experiment.year, treatment.year, recov.year, pres.abs, max.cover)

# Clean Env ####
rm(chy_sp, chy_fill, hys_sp, hys_fill, knz_sp, knz_fill, sbk_sp, sbk_fill, sbl_sp, sbl_fill, sgs_sp, sgs_fill)
