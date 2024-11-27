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
    ## the version of SEV data in the sev_download folder was downloaded on 11/5/2024 from EDI 
    ## (https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sev.298.209677)

# Clean ####
## SEV Sites ####
sort(unique(sev_edge$kartez))
sort(unique(sev_edge$genus))

empty <- sev_edge %>%
  filter(kartez == "EMPTY")
#none <- sev_edge %>%
 # filter(kartez == "NONE") ## this is actually a species, don't filter it out!!

## each of the unknowns shows up once
unk <- sev_edge %>%
  filter(kartez == "UNKNOWN", treatment != "D")
## 11 unknowns in the two relevant treatments

### sp cleaning decisions
    ## remove unknowns
    ## make case by case decision on species identified by genus but not to species level

rm_kartez <- c("EMPTY", "UNKNOWN") ## remove unknowns and empty
rm_k2 = c(rm_kartez, "SPHAE")

### quantify unknowns ####
sev_unknowns = sev_edge %>%
  filter(kartez %in% rm_k2, treatment != "D", kartez != "EMPTY")
## 12 obs in the Control and Event Reduction Treats

### first mods ####
unique(sev_edge$year)
## 2012 = pre-treatment year
## according to meta-data drought was applied 2013-2019

## OLD Q here 
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
  filter(treatment != "D", ## remove monsoon timing treatment 
         !kartez %in% rm_kartez) ## remove unknowns/empty species

### clean species ####
## find species with genus id present, but no sp epithet id
unk_epithet = sev_temp %>%
  filter(is.na(sp.epithet))

unique(unk_epithet$genus)
## 3 genera with unidentified sp epithets
    ## "Sporobolus"  "Astragalus"  "Sphaeralcea"

astrag = sev_temp %>%
  filter(genus == "Astragalus") %>%
  group_by(site, species, year) %>%
  summarise(num.obs = n())
## seems like astragalus was perhaps not identified by species until 2016
## lump "Astragalus_missouriensis", "Astragalus_NA", and "Astragalus_nuttallianus" all as "Astragalus_sp"
## kartez to ASTRA

sphaer = sev_temp %>%
  filter(genus == "Sphaeralcea") %>%
  group_by(site, species, year) %>%
  summarise(num.obs = n())
## only one observation of Sphaeralcea_NA in 2020; probably best to remove this one?

sporob = sev_temp %>%
  filter(genus == "Sporobolus") %>%
  group_by(site, species, year) %>%
  summarise(num.obs = n())
## lump at genus level; 
## "Sporobolus_contractus", "Sporobolus_cryptandrus", "Sporobolus_flexuosus", and "Sporobolus_NA" all as "Sporobolus_sp"
## kartez to SPORO
## can only determine species by reproductive structures, so if no seedheads in a year sp.epithet is marked as NA

### second mods ####
sev_clean = sev_temp %>%  
  filter(species != "Sphaeralcea_NA") %>%
  
  ## lump sporobolus species
  mutate(spcode = ifelse(genus == "Sporobolus", "SPOSP", spcode),
         species = ifelse(genus == "Sporobolus", "Sporobolus_sp", species),
         kartez = ifelse(genus == "Sporobolus", "SPORO", kartez),
         
  ## lump astragalus species
  spcode = ifelse(genus == "Astragalus", "ASTSP", spcode),
  species = ifelse(genus == "Astragalus", "Astragalus_sp", species),
  kartez = ifelse(genus == "Astragalus", "ASTRA", kartez)) %>%

  ## capitalize spcode
  mutate(across(c(spcode), toupper), 
         
         ## fix a kartez code to prevent row from duplicating
         kartez = ifelse(species == "Glandularia_bipinnatifida", "GLBI2", kartez)) %>% 
  
  ## group by everything except season; this lets us take the maximum value of the season in the same calendar year.
  group_by(site, treatment, block, plot, subplot, year, experiment.year, treatment.year, species, spcode, kartez) %>% 
  ## get max cover
  summarise(max.cover = max(cover)) %>%
  
  ## change from E -> D for more intuitive notation
  mutate(treatment = ifelse(treatment == "E", "D", treatment)) %>% 
  ungroup()

### fill 0's ####
#### sbk ####
## split by sites and fill 0's
sbk_sub = sev_clean %>%
  filter(site == "SBK") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_eriopoda:Psilostrophe_tagetina, names_to = "species", values_to = "max.cover") 

## 12 years * 20 plots * (4 subs (for 7 years) 2 subs for 5 years) * 81 species (post-lumping)
(7*20*4*81) + (5*20*2*81)

check_fill = sbk_sub %>%
  group_by(year, block, plot, subplot) %>%
  summarise(num.sp = n())
## 81 observations in every block/plot/sub, good

check_fill2 = sbk_sub %>%
  group_by(species) %>%
  summarise(num.obs = n())
## every species has same num obs, good

testsub = sbk_sub %>%
  group_by(year, block, plot) %>%
  summarise(num.sub = n())

weirdsubs = testsub %>%
  filter(num.sub == 243)
## okay, some of the plots in 2020 & perhaps one in 2022 had 3 subplots counted instead of 2

#### sbl ####
sbl_sub = sev_clean %>%
  filter(site == "SBL") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_gracilis:Ipomoea_costellata, names_to = "species", values_to = "max.cover")

## 12 years 
12*20*4*97 - (5*20*2*97)

check_fillsbl = sbl_sub %>%
  group_by(year, block, plot, subplot) %>%
  summarise(num.sp = n())
## 97 observations in every block/plot/sub, good

check_fillsbl2 = sbl_sub %>%
  group_by(species) %>%
  summarise(num.obs = n())
## every species has same num obs, good

testsub = sbl_sub %>%
  group_by(year, block, plot) %>%
  summarise(num.sub = n())
## okay, these are either 388/194

#### combine ####
sev_comb = rbind(sbk_sub, sbl_sub) %>%
  mutate(pres.abs = ifelse(max.cover > 0, 1, 0)) %>%
  select(year, site, treatment, block, plot, subplot, species, experiment.year, treatment.year, pres.abs, max.cover)

## North Sites ####
### check unknowns ####
sort(unique(north_edge$Species))

unk_codes = c("UNKFCHY1", "UNKFCHY2", "UNKFCHY3", "UNKFCHY4", "UNKFCHY5", "UNKFCHY6", "UNKFCHY7", "UNKFCHY8", "UNKFHYS1", "UNKFHYS2", "UNKFHYS3", "UNKFHYS4","UNKFHYS5", "UNKFHYS7", "UNKFHYS8", "UNKFKNZ1", "UNKFKNZ2", "UNKFKNZ3", "UNKFSGS1", "UNKFSGS2", "UNKFSGS3", "UNKGRHYS1", "UNKGRHYS2","UNKHYS")

north_unk_codes = north_edge %>%
  filter(Species %in% unk_codes)

## okay the codes of these seem to be UNK = unknown; F or GR for forb or grass? then site code; then numbers 1+ for the number of unknowns.

## remove unknowns
rm_no_info = c("oxytopis_like_legume", "seedling_unknown", "UK_Fuzzy_Aster", "UK_Tall_Phlox", "unk_Alien", "unk_alternate_leaf_forb", "unk_alternate_strong_midvein_hairy_margin", "UNK_Aster_rosette", "unk_Clover", "unk_fall_opposite_leaf", "unk_forb_soft_velvet", "unk_juicy_forb", "unk_Lepidium_like_forb", "unk_Milky_waxy", "unk_opposite_leaf", "Unk_overlapping_alt",  "unk_Primrose_like", "unk_Red_edged_forb", "unk_rush_unknown",  "unk_Three_Leaf_Unknown_forb", "UNKFCHY1", "UNKFCHY2", "UNKFCHY3", "UNKFCHY4", "UNKFCHY5", "UNKFCHY6", "UNKFCHY7", "UNKFCHY8", "UNKFHYS1", "UNKFHYS2", "UNKFHYS3", "UNKFHYS4","UNKFHYS5", "UNKFHYS7", "UNKFHYS8", "UNKFKNZ1", "UNKFKNZ2", "UNKFKNZ3",  "unkforb_opp_Lvs", "UNKFSGS1", "UNKFSGS2", "UNKFSGS3", "UNKGRHYS1", "UNKGRHYS2","UNKHYS", "unknown", "Unknown_dry_sad","unknown_forb", "Unknown_forb", "unknown_forb_tooth", "Unknown_grass", "Unknown_linear_lvs", "Unknown_milky_waxy", "Unknown_pilos_forb", "unknown_pinnately_lobed", "Unknown_rosette", "Unknown_Seedling", "unknown_shiny_alternate", "unknown_short_alternate", "Unknown_whorled_linear", "Unknown_woody", "UNKTRKNZ1", "UNKTRKNZ2", "blob_unknown", "Ulmus_sp.", "NA_NA", "Ulmus_americana", "unk_mustard_unknown")

## look into further
unk_some_info = c("UK_onagraceac", "UK_poa", "unk_Aristida", "unk_Artemisia_ludoviciana", "unk_astragalus_oxytropis", "unk_Eriogonum_Hays", "unk_Oenothera", "unk_oenotheria", "unk_Oerothera_rosette","unk_Oxytropis_sp.", "unk_Sonchus_seedling", "unk_Stipa_veridas", "unk_Tall_astragulus", "unk_Townsendia_grandiflora", "Unknown_Cirsium", "Unknown_ericoides_small", "Unknown_Erysimum", "unknown_machearanthera","Unknown_ranunculus", "huge_penstemon", "panicum_unknown")

### first mods ####
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
           sapply(head,1),
         
         ## change ASSY to Asclepias syriaca
         species = ifelse(species == "ASSY", "Asclepias_syriaca", species),
         genus = ifelse(species == "ASSY", "Asclepias", genus),
         sp.ep = ifelse(species == "ASSY", "syriaca", sp.ep)) %>%
  
  filter(treatment != "int", year > 2012, !species %in% rm_no_info) ## remove 2012 as was drought pre-treat year

### clean species ####
unk_epithetN = north_temp %>%
  filter(is.na(sp.ep) | sp.ep %in% c("sp.", "seedling", "unknown"))

sort(unique(unk_epithetN$genus))
## "Asclepias"   "Astragalus"  "Chenopodium" "Chloris"     "Cirsium"     "Croton"      "Eleocharis"  "Euphorbia"   "Melilotus" "Oenothera"   "Orobanche"   "Paronychia"  "Silene"      "Sporobolus"  "Triodanis"

## "Festuca" panicum, unk

#### asclep ####
asclep = north_temp %>%
  filter(genus == "Asclepias") %>% #, site == "HYS", species != "Asclepias_seedling") %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(asclep$site, asclep$species)
## Asclepias_seedling; 1 obs at HYS
## Asclepias_sp.; 8 obs at HYS
## other HYS Asclepias species: Asclepias_asperula, Asclepias_stenophylla, Asclepias_tuberosa, Asclepias_viridis

## KNZ Asclepias species look well id'ed

asclepHYS = asclep %>%
  filter(site == "HYS") %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

## the problem Asclepias are at HYS
## will have to remove Asclepias seedling, that's low cover & should be no problem
## Asclepias species is more prevalent...

ggplot(asclepHYS, aes(x=year, y=plot, color = species)) +
  #geom_jitter(size = 4) +
  geom_point(size = 4) +
  #geom_line() +
  facet_wrap(~species, ncol = 6, nrow = 1) 

## plots, 1,7,8,17,28 all have 'Asclepias_sp'
## doesn't seem like an easy way to separate it out; will have to just remove Asclepias_sp

#### astrag ####
astrag = north_temp %>%
  filter(genus %in% c("Astragalus", "ASOX")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(astrag$site, astrag$species)
## ASOX shows up only at SGS - handle this site separately to look at both Astragalus and Oxytropis species together

astragHC = north_temp %>%
  filter(genus %in% c("Astragalus", "ASOX"), site %in% c("HYS", "CHY")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(astragHC$site, astragHC$species)
## at HYS most 

ggplot(astragHC, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_grid(site~species) 
#ggsave("figures/Nov2024_postmeeting/CHY_HYS_astrag_unknowns.png", width = 12, height = 3.5)

asoxSGS = north_temp %>%
  filter(genus %in% c("Astragalus", "ASOX", "Oxytropis"), site %in% c("SGS"))
table(asoxSGS$site, asoxSGS$species)

ggplot(asoxSGS, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_wrap(~species, ncol = 4, nrow = 2)
##ggsave("figures/Nov2024_postmeeting/SGS_asox_unknowns.png", width = 11, height = 5)

#### chenop ####
chenop = north_temp %>%
  filter(genus %in% c("Chenopodium")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(chenop$site, chenop$species)
## this is left at genus level at all sites where it is found; leave as is

#### chloris ####
chloris = north_temp %>%
  filter(genus %in% c("Chloris")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(chloris$site, chloris$species)
## this is left at genus level at all sites where it is found; leave as is

#### cirsium ####
cirsium = north_temp %>%
  filter(genus %in% c("Circium", "Cirsium")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(cirsium$site, cirsium$species)

## Cirsium sp seems to show up only at KNZ
cirsiumKNZ = north_temp %>%
  filter(genus == "Cirsium", site == "KNZ")

ggplot(cirsiumKNZ, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_wrap(~species) 
## ggsave("figures/Nov2024_postmeeting/KNZ_cirsium_unknowns.png", width = 6, height = 3)

#### croton ####
croton = north_temp %>%
  filter(genus %in% c("Croton")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(croton$site, croton$species)
## this is left at genus level at all sites where it is found; leave as is

#### eleoch ####
eleoch = north_temp %>%
  filter(genus %in% c("Eleocharis")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(eleoch$site, eleoch$species)
## this is left at genus level at all sites where it is found; leave as is

#### euphorb ####
euphorb = north_temp %>%
  filter(genus %in% c("Euphorbia")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(euphorb$site, euphorb$species)
## this is left at genus level SGS; at KNZ & HYS there is one other less abundant Euphorbia ID'ed to species

ggplot(euphorb, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_grid(site~species)
##ggsave("figures/Nov2024_postmeeting/euphorb_unknowns.png", width = 9, height = 3.5)

#### melilo ####
melilo = north_temp %>%
  filter(genus %in% c("Melilotus")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(melilo$site, melilo$species)
## this is left at genus level at all sites where it is found; leave as is

#### oenoth ####
oenoth = north_temp %>%
  filter(genus %in% c("Oenothera")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(oenoth$site, oenoth$species)

ggplot(oenoth, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_grid(site~species)
ggsave("figures/Nov2024_postmeeting/oenoth_unknowns.png", width = 12, height = 8)

#### oroban ####
oroban = north_temp %>%
  filter(genus %in% c("Orobanche")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(oroban$site, oroban$species)
## this is left at genus level at all sites where it is found; leave as is

#### parony ####
parony = north_temp %>%
  filter(genus %in% c("Paronychia")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(parony$site, parony$species)
## this is left at genus level at all sites where it is found; leave as is

#### silene ####
silene = north_temp %>%
  filter(genus %in% c("Silene")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(silene$site, silene$species)
## leave as is; Silene_sp found only at CHY

#### sporobN ####
sporobN = north_temp %>%
  filter(genus %in% c("Sporobolus")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(sporobN$site, sporobN$species)
## leave as is; Sporobolus_sp found only at CHY where no other Sporobolus are ID'ed to species

#### trioda ####
trioda = north_temp %>%
  filter(genus %in% c("Triodanis")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(trioda$site, trioda$species)
## this is left at genus level at all sites where it is found; leave as is

#### festuca ####
## "Festuca_unknown"  
festuca = north_temp %>%
  filter(genus == "Festuca")
## only one specieis in the genus festuca; leave it in!

#### panic ####
panic = north_temp %>%
  filter(genus %in% c("panicum", "Panicum")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())

table(panic$site, panic$species)
## remove panicum_unknown as there are only 2 observations but two other species are ID'ed to species level



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
## 62

ggplot(north_unknowns, aes(x=max.cover)) +
  geom_histogram() +
  facet_wrap(~Site, ncol = 4, nrow = 1) +
  xlab("Unknown Species Cover") +
  ylab("Count")
#ggsave("figures/final_figs/supp/north_sites_unknowns.png", width = 6, height = 2.5)


### second mods ####
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
