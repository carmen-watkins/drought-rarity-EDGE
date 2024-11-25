# Header #### 
## Script name: Clean cover data, fill zeros
##
## Purpose of script: Clean & standardize cover data at each EDGE site so that it's ready for analyses and sites can be combined into one dataframe. Additionally, fill zeros into unrecorded subplots to account for spatial rarity more accurately.
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
## SEV ####
sort(unique(sev_edge$kartez))
sort(unique(sev_edge$genus))

empty <- sev_edge %>%
  filter(kartez == "EMPTY")
none <- sev_edge %>%
  filter(kartez == "NONE") ## this is actually a species, don't filter it out!!

## each of the unknowns shows up once
unk <- sev_edge %>%
  filter(kartez == "UNKNOWN", treatment != "D")
## 11 unknowns in the two relevant treatments

unk2 = sev_edge %>%
  filter(kartez == "UNKFORB1")

### sp questions
## should we keep species with 'NA' for specific epithet? - leave in
## what should we do with unknowns? - remove

rm_kartez <- c("EMPTY", "UNKFORB1", "UNKNOWN") ## remove unknowns and empty

rm_k2 = c(rm_kartez, "SPHAE")

### quantify unknowns ####
sev_unknowns = sev_edge %>%
  filter(kartez %in% rm_k2, treatment != "D", kartez != "EMPTY")
## 22 obs in the Control and Event Reduction Treats

### make mods ####
unique(sev_edge$year)
## 2012 = pre-treatment year
## 2013, 2014, 2015, 2016 = drought years
## Q HERE 
## do I calculate the max cover by comparing spring and fall of diff or the same years?
## from readings, seems like growing season is ~Apr - Oct so it should be same year 
sev_temp <- sev_edge %>%
  mutate(site = ifelse(site == "EDGE_black", "SBK", "SBL"), ## fix site code
         subplot = quad, ## rename quad as subplot to match north sites
         spcode = paste0(substr(genus, 1, 3), substr(sp.epithet, 1, 3)), ## make 6 letter sp codes
         species = paste0(genus, "_", sp.epithet)) %>%
  mutate(experiment.year = year - 2012, 
         treatment.year = ifelse(year == 2012, "pre-treatment", 
                                 ifelse((2012 < year) & (year < 2020), "drought", "recovery"))) %>%
  ## according to meta-data drought was applied 2013-2019
  
  filter(treatment != "D", ## remove monsoon timing treatment 
         !kartez %in% rm_kartez) ## remove unknowns/empty species

## look for species with genus id but sp epithet is NA
sort(unique(sev_temp$species))
## "Astragalus_missouriensis"     "Astragalus_NA"                "Astragalus_nuttallianus" 

## "Sphaeralcea_leptophylla" "Sphaeralcea_NA" "Sphaeralcea_polychroma" "Sphaeralcea_wrightii" 

## "Sporobolus_contractus"   "Sporobolus_cryptandrus"       "Sporobolus_flexuosus"         "Sporobolus_NA" 

astrag = sev_temp %>%
  filter(genus == "Astragalus") %>%
  group_by(site, species, year) %>%
  summarise(num.obs = n())
## seems like astragalus was perhaps not identified by species until 2016

sphaer = sev_temp %>%
  filter(genus == "Sphaeralcea") %>%
  group_by(site, species, year) %>%
  summarise(num.obs = n())
## only one observation of Sphaeralcea_NA in 2020; probably best to remove this one

sphaer2 = sev_temp %>%
  filter(species == "Sphaeralcea_NA")
## only one observation of Sphaeralcea_NA in 2020; probably best to remove this one

sporob = sev_temp %>%
  filter(genus == "Sporobolus") %>%
  group_by(site, species, year) %>%
  summarise(num.obs = n())
## Sporobolus NA is the most abundant one; need to determine how best to handle this; perhaps consider lumping at genus level unless someone with experiment knowledge can confirm that NA is a distinct species...

sev_clean = sev_temp %>%  
  filter(species != "Sphaeralcea_NA") %>%
  
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
sort(unique(north_edge$Species))

## remove unknowns
rm <- c("oxytopis_like_legume", "seedling_unknown", "UK_Fuzzy_Aster", "UK_onagraceac", "UK_poa", "UK_Tall_Phlox", "unk_Alien", "unk_alternate_leaf_forb", "unk_alternate_strong_midvein_hairy_margin", "unk_Aristida", "unk_Artemisia_ludoviciana", "UNK_Aster_rosette", "unk_astragalus_oxytropis", "unk_Clover", "unk_Eriogonum_Hays", "unk_fall_opposite_leaf", "unk_forb_soft_velvet", "unk_juicy_forb", "unk_Lepidium_like_forb", "unk_Milky_waxy", "unk_Oenothera", "unk_oenotheria", "unk_Oerothera_rosette", "unk_opposite_leaf", "Unk_overlapping_alt", "unk_Oxytropis_sp.", "unk_Primrose_like", "unk_Red_edged_forb", "unk_rush_unknown", "unk_Sonchus_seedling", "unk_Stipa_veridas", "unk_Tall_astragulus", "unk_Three_Leaf_Unknown_forb", "unk_Townsendia_grandiflora", "UNKFCHY1", "UNKFCHY2", "UNKFCHY3", "UNKFCHY4", "UNKFCHY5", "UNKFCHY6", "UNKFCHY7", "UNKFCHY8", "UNKFHYS1", "UNKFHYS2", "UNKFHYS3", "UNKFHYS4","UNKFHYS5", "UNKFHYS7", "UNKFHYS8", "UNKFKNZ1", "UNKFKNZ2", "UNKFKNZ3",  "unkforb_opp_Lvs", "UNKFSGS1", "UNKFSGS2", "UNKFSGS3", "UNKGRHYS1", "UNKGRHYS2","UNKHYS", "unknown", "Unknown_Cirsium", "Unknown_dry_sad", "Unknown_ericoides_small", "Unknown_Erysimum", "unknown_forb", "Unknown_forb", "unknown_forb_tooth", "Unknown_grass", "Unknown_linear_lvs", "unknown_machearanthera", "Unknown_milky_waxy", "Unknown_pilos_forb", "unknown_pinnately_lobed", "Unknown_ranunculus", "Unknown_rosette", "Unknown_Seedling", "unknown_shiny_alternate", "unknown_short_alternate", "Unknown_whorled_linear", "Unknown_woody", "UNKTRKNZ1", "UNKTRKNZ2", "huge_penstemon", "blob_unknown", "Ulmus_sp.", "NA_NA", "Ulmus_americana")

rm2 = c(rm, "Asclepias_seedling", "Asclepias_sp", "Cirsium_sp.", "Festuca_unknown", "panicum_unknown", "unk_mustard_unknown")

### quantify unknowns ####
north_unknowns = north_edge %>%
  filter(Species %in% rm2, 
         max.cover > 0,
         Species != "Ulmus_americana",
         Species != "Ulmus_sp.", 
         Year > 2012,
         Trt != "int" 
  ) %>%
  mutate(Site = fct_relevel(Site, "KNZ", "HYS", "CHY", "SGS"),
         cov_below1 = ifelse(max.cover <= 2, 1, 0))

sum(north_unknowns$cov_below1)/nrow(north_unknowns)
## 0.9036

length(north_unknowns$Site)
## 166

length(unique(north_unknowns$Species))
## 57

ggplot(north_unknowns, aes(x=max.cover)) +
  geom_histogram() +
  facet_wrap(~Site, ncol = 4, nrow = 1) +
  xlab("Unknown Species Cover") +
  ylab("Count")

#ggsave("figures/final_figs/supp/north_sites_unknowns.png", width = 6, height = 2.5)

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
  filter(treatment != "int", year > 2012, !species %in% rm) ## remove 2012 as was drought pre-treat year

sort(unique(north_temp$species))
# "Asclepias_asperula"           "Asclepias_seedling"           "Asclepias_sp."                "Asclepias_stenophylla"  [21] "Asclepias_sullivantii"        "Asclepias_tuberosa"           "Asclepias_verticillata"       "Asclepias_viridiflora"  "Asclepias_viridis"



asclep = north_temp %>%
  filter(genus == "Asclepias", site == "HYS", species != "Asclepias_seedling") %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
## the problem Asclepias are at HYS
## will have to remove Asclepias seedling, that's low cover & should be no problem
## Asclepias species is more prevalent...

ggplot(asclep, aes(x=year, y=plot, color = species)) +
  geom_jitter(size = 4) +
  #geom_line() +
  facet_wrap(~plot)

## plots, 1,7,8,17,28 all have 'Asclepias_sp'
## doesn't seem like an easy way to separate it out; will have to just remove Asclepias_sp

## "ASOX" "ASSY"
AS_sp = north_temp %>%
  filter(species %in% c("ASOX", "ASSY"))

Assp = north_edge %>%
  filter(Species %in% c("ASOX", "ASSY"))

## ASSY seems to be Asclepias syriaca - this is the code on USDA plant database website

## "Astragalus_drummondii"        "Astragalus_fluxuosus"         "Astragalus_gracilis"     "Astragalus_laxmanii"          "Astragalus_lotiflorus"        "Astragalus_missouriensis"     "Astragalus_mollissimus"    "Astragalus_shortianus"        "Astragalus_sp."               "Astragalus_unknown"           "Astragulus_crassicarpus"  
astrag = north_temp %>%
  filter(genus == "Astragalus", site != "SGS", site != "CHY") %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
## Astragalus_unknown shows up only one time, remove this
## remove Astragalus_sp. at CHY for sure
## consider lumping Astragalus sp and Astragalus_missouriensis and Astragalus_unknown at HYS

ggplot(astrag, aes(x=year, y=num.obs, color = species)) +
  geom_jitter(size = 4, width = 0, height = 0.09) +
  #geom_line() +
  #geom_line() +
  facet_wrap(~plot) 


## "Circium_ochochrocentrum"      "Cirsium_altissimum"   "Cirsium_sp."                  "Cirsium_undulatum"
cirsi = north_temp %>%
  filter(genus %in% c("Circium", "Cirsium")) %>%
  group_by(site, species,) %>%
  summarise(num.obs = n())
## remove Cirsium species -- just 4 obs

## "Euphorbia_marginata"          "Euphorbia_sp."                "Euphorbiadavidii"     
euphorb = north_temp %>%
  filter(genus == "Euphorbia") %>%
  group_by(site, species,) %>%
  summarise(num.obs = n())

## Euphorbia sp is super prevalent; maybe they just lumped most of these to begin with...


## "Festuca_unknown"  
festuca = north_temp %>%
  filter(genus == "Festuca")
## get rid of this = only in one subplot


## "Oenothera_albicaulis"         "Oenothera_coronopifolia"      "Oenothera_sp." "Oenothera_speciosa"           "Oenothera_suffrutescens" 

oenoth = north_temp %>%
  filter(genus == "Oenothera") %>%
  group_by(site, species,) %>%
  summarise(num.obs = n())

## DECISION HERE ####

unique(north_temp$Species)
## "Panicum_capillare"            "panicum_unknown"              "Panicum_virgatum"  

panic = north_temp %>%
  filter(genus == "panicum") %>%
  group_by(site, species,) %>%
  summarise(num.obs = n())

panicum = north_temp %>%
  filter(genus == "Panicum") %>%
  group_by(site, species,) %>%
  summarise(num.obs = n())

## "Silene_antirrhina"            "Silene_sp."
silene = north_temp %>%
  filter(genus == "Silene") %>%
  group_by(site, species,) %>%
  summarise(num.obs = n())
## keep all obs in; silene_sp shows up at just one site that has no other silene at it

## "Sporobolus_asper"  "Sporobolus_cryptandrus"       "Sporobolus_heterolepis"       "Sporobolus_sp."
sporobN = north_temp %>%
  filter(genus == "Sporobolus") %>%
  group_by(site, species,) %>%
  summarise(num.obs = n())
## keep all obs in; sporobolus_sp shows up at just one site that has no other sporob at it

## "unk_mustard_unknown"

north_clean = north_temp %>%  
  
  filter(!species %in% c("Asclepias_seedling", "Asclepias_sp", "Cirsium_sp.", "Festuca_unknown", "panicum_unknown", "unk_mustard_unknown", "ASOX")) %>%
  
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
  pivot_longer(Achillea_millefolium:Panicum_capillare, names_to = "species", values_to = "max.cover") 

## 9-years * 20 plots * 4 subplots * 78 species = 56160; this checks out for one obs per species per subplot per years

hys_sub = north_clean %>%
  filter(site == "HYS") %>%
  ungroup() %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Achillea_millefolium:Croton_sp., names_to = "species", values_to = "max.cover")

## 9-years * 20 plots * 4 subplots * 107 species = 77040; this checks out for one obs per species per subplot per years

chy_sub = north_clean %>%
  filter(site == "CHY") %>%
  ungroup() %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Allium_textile:Sporobolus_sp., names_to = "species", values_to = "max.cover") 

## 9-years * 20 plots * 4 subplots * 79 species = 56880; this checks out for one obs per species per subplot per years

sgs_sub = north_clean %>%
  filter(site == "SGS") %>%
  ungroup() %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Aristida_purpurea:Picradeniopsis_oppositifolia, names_to = "species", values_to = "max.cover")

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
