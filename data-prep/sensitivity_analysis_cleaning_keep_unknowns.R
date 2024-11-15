# Header #### 
## Script name: Clean EDGE Data

## zero - filled

## test analyses w/o removing unknowns
##
## Purpose of script: Clean & standardize cover data at each EDGE site so that it's ready for analyses and sites can be combined into one dataframe.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up Env ####
## load packages
library(tidyverse)
theme_set(theme_classic())

## Read in Data ####
## northern edge
north_edge <- read.csv("data/north_edge_sites/spcomp_subplot_names.csv")
## max cover combines the two seasonal samplings and takes the largest of these, I believe. 

north_spkey <- read.csv("data/north_edge_sites/spnames_code.csv")

## sev edge 
sev_edge <- read.csv("data/sev_download/sev298_NPP_edge_biomass.csv")
## the version of SEV data in the sev_download folder was downloaded on 11/5/2024 from EDI (https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sev.298.209677)

# Clean ####
unique(sev_edge$kartez)


## SEV ####
## each of the unknowns shows up once
  
### make mods ####
sev_temp <- sev_edge %>%
  mutate(site = ifelse(site == "EDGE_black", "SBK", "SBL"), ## fix site code
         subplot = quad, ## rename quad as subplot to match north sites
         spcode = paste0(substr(genus, 1, 3), substr(sp.epithet, 1, 3)), ## make 6 letter sp codes
         species = paste0(genus, "_", sp.epithet)) %>%
  mutate(experiment.year = year - 2012, 
         treatment.year = ifelse(year == 2012, "pre-treatment", 
                                 ifelse((2012 < year) & (year < 2020), "drought", "recovery"))) %>%
  ## according to meta-data drought was applied 2013-2019
  
  filter(treatment != "D", 
         !kartez == "EMPTY") ## remove monsoon timing treatment 
       ##  !kartez %in% rm_kartez) ## remove unknowns/empty species

sev_clean = sev_temp %>%  
  #filter(species != "Sphaeralcea_NA") %>%
  
  mutate(across(c(spcode), toupper), ## capitalize
         kartez = ifelse(species == "Glandularia_bipinnatifida", "GLBI2", kartez)) %>% ## fix a kartez code to prevent row from duplicating
  
  group_by(site, treatment, block, plot, subplot, year, experiment.year, treatment.year, species, spcode, kartez) %>% ## grouping by everything except season; this lets us take the maximum value of the season in the same calendar year.
  summarise(max.cover = max(cover)) %>%
  
  mutate(treatment = ifelse(treatment == "E", "D", treatment)) %>% ## change from E -> D for more intuitive notation
  ungroup()

## split by sites and fill 0's
sbk_sub = sev_clean %>%
  filter(site == "SBK") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_eriopoda:Psilostrophe_tagetina, names_to = "species", values_to = "max.cover") 

## 12 years * 20 plots * (4 subs (for 7 years) 2 subs for 5 years) * 86 species

test = sbk_sub %>%
  group_by(year, block, plot, subplot) %>%
  summarise(num.sp = n())

test2 = sbk_sub %>%
  group_by(species) %>%
  summarise(num.obs = n())

testsub = sbk_sub %>%
  group_by(year, block, plot) %>%
  summarise(num.sub = n())

weirdsubs = testsub %>%
  filter(num.sub == 258)

weirdsubs2 = sev_clean %>%
  filter(year == 2020, 
         block %in% c(3, 7, 8, 9, 10), 
         plot %in% c(27, 28, 6, 3,2, 1))

testws2 = weirdsubs2 %>%
  group_by(block, plot, species) %>%
  summarise(num.sub = n())

## okay, some of the plots in 2020 & perhaps one in 2022 had 3 subplots counted instead of 2

(12*20*4*86) - (5*20*2*86)

## 82560 - (2*20*5*86)
82560-17200

sbl_sub = sev_clean %>%
  filter(site == "SBL") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_gracilis:Ipomoea_costellata, names_to = "species", values_to = "max.cover")

## 12 years 
12*20*4*102 - (5*20*2*102)

test = sbl_sub %>%
  group_by(year, block, plot, subplot) %>%
  summarise(num.sp = n())

test2 = sbl_sub %>%
  group_by(species) %>%
  summarise(num.obs = n())

testsub = sbl_sub %>%
  group_by(year, block, plot) %>%
  summarise(num.sub = n())

## okay, these are either 408/204

## combine 0-filled data
sev_comb = rbind(sbk_sub, sbl_sub) %>%
  mutate(pres.abs = ifelse(max.cover > 0, 1, 0)) %>%
  select(year, site, treatment, block, plot, subplot, species, experiment.year, treatment.year, pres.abs, max.cover)


## North EDGE ####
### sp notes ####
### clean data ####
north_temp <- north_edge %>%
  mutate(site = Site, ## fix column names
         plot = Plot,
         block = Block, 
         treatment = Trt, 
         subplot = Subplot,
         species = Species,
         kartez = NA,
         year = Year,
         genuscode = toupper(substr(Species, 1, 3)), ## make a 6 letter species code column
         sp.ep = strsplit(Species, "_") %>%
           sapply(tail, 1),
         spepcode = toupper(substr(sp.ep, 1, 3)),
         spcode = paste0(genuscode, spepcode),
         genus = strsplit(Species, "_") %>%
           sapply(head,1)) %>%
  filter(treatment != "int", year > 2012 ) ## remove 2012 as was drought pre-treat year

north_clean = north_temp %>%  

  mutate(treatment = ifelse(treatment == "chr", "D", "C"), 
         experiment.year = year - 2013, ## 2013 is pre-treat year
         treatment.year = ifelse(year == 2013, "pre-treatment", 
                                 ifelse((2013 < year) & (year < 2018), "drought", "recovery"))) %>%
  select(-spcode, -kartez, -Spcode, -Species, -sp.ep, -spepcode, -genus, -genuscode, -Site, -Plot, -Block, -Trt, -Subplot, -Year)

## separate and fill with 0's  
knz_sub = north_clean %>%
  filter(site == "KNZ") %>%
  ungroup() %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Achillea_millefolium:unk_rush_unknown, names_to = "species", values_to = "max.cover") 

## 9-years * 20 plots * 4 subplots * 78 species = 56160; this checks out for one obs per species per subplot per years

hys_sub = north_clean %>%
  filter(site == "HYS") %>%
  ungroup() %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Achillea_millefolium:unknown_forb, names_to = "species", values_to = "max.cover")

## 9-years * 20 plots * 4 subplots * 107 species = 77040; this checks out for one obs per species per subplot per years

chy_sub = north_clean %>%
  filter(site == "CHY") %>%
  ungroup() %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Allium_textile:UNK_Aster_rosette, names_to = "species", values_to = "max.cover") 

## 9-years * 20 plots * 4 subplots * 79 species = 56880; this checks out for one obs per species per subplot per years

sgs_sub = north_clean %>%
  filter(site == "SGS") %>%
  ungroup() %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Aristida_purpurea:ASOX, names_to = "species", values_to = "max.cover")

## 9-years * 20 plots * 4 subplots * 61 species = 43920; this checks out for one obs per species per subplot per years

## combine zero filled sites
north_comb = rbind(knz_sub, hys_sub, chy_sub, sgs_sub) %>%
  mutate(pres.abs = ifelse(max.cover > 0, 1, 0)) %>%
  select(year, site, treatment, block, plot, subplot, species, experiment.year, treatment.year, pres.abs, max.cover)

## Merge ####
#colnames(north_clean) 
#colnames(sev_clean)
edge_all <- rbind(north_comb, sev_comb)

# clean up env ####
rm(north_clean, north_edge, sev_clean, sev_edge, north_spkey, empty, none,  unk, rm_kartez, chy, hay, knz, sgs, summary, summary2, blue_3_20_2019, duplicates, rm, sbk, sbl, sev_unknowns, sev_plot_check, north_unknowns, north_plot_check, AS_sp, asclep, Assp, astrag, cirsi, euphorb, festuca, north_temp, oenoth, panic, panicum, sev_temp, silene, sphaer, sporob, sporobN, north_comb, sbk_sub, sbl_sub, sev_comb, sphaer2, unk2, rm2, rm_k2, knz_sub, hys_sub, chy_sub, sgs_sub)
