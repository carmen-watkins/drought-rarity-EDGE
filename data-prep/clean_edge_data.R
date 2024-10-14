# Header #### 
## Script name: Clean EDGE Data
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
north_edge <- read.csv("data/spcomp_subplot_names.csv")
    ## max cover combines the two seasonal samplings and takes the largest of these, I believe. 
    ## Don't think this has been done with the sev site yet

north_spkey <- read.csv("data/spnames_code.csv")

## sev edge 
sev_edge <- read.csv("data/sev298_NPP_edge_biomass.csv")
    ## there is cover data here also
    ## would be good to talk to SEV EDGE expert and confirm a few things about the data here

# Explore Data ####
## Exp Design ####

blocks <- sev_edge %>%
  group_by(site, block) %>%
  summarise(plot.num = length(unique(plot)))
## 3 plots per block, each corresponding to one treatment (drought, control, drought timing)

ggplot(sev_edge, aes(x=block, y=plot)) +
  geom_point() +
  facet_wrap(~site)
#ggsave("preliminary_figs/sev_plots_per_block.png", width = 6, height = 3)

ggplot(north_edge, aes(x=as.factor(Block), y=Plot)) +
  geom_point() +
  facet_wrap(~Site)
#ggsave("preliminary_figs/north_edge_plots_per_block.png", width = 6, height = 5)

ggplot(north_edge, aes(x=max.cover))+
  geom_histogram()
## maximum cover of one species is 100

high.cov = north_edge %>%
  filter(max.cover > 95)

## how many cover values per plot in a single year? i.e. is one subplot measured per plot or multiple ?

north_plot_check = north_edge %>%
  mutate(uniqueID = paste0(Site, "B", Block, "P", Plot, Trt, Subplot)) %>%
  filter(Site == "KNZ",
         Block == 1, 
         Trt != "int")
 
unique(north_plot_check$uniqueID)
## yep, all four subplot per plot

sev_plot_check = sev_edge %>%
  mutate(uniqueID = paste0(site, "B", block, "P", plot, treatment, subplot)) %>%
  filter(site == "EDGE_black",
         block == 3, 
         treatment != "D")

unique(sev_plot_check$uniqueID)


## Column Names ####
colnames(north_edge)
colnames(north_spkey)
colnames(sev_edge)

## Column Vals ####
unique(north_edge$Spcode)
unique(north_edge$Trt)
## int = intense drought, chr = chronic drought, con = control 
    ## keep chr and con?
unique(sev_edge$treatment)
## C = control; E = event reduction by 66%; D = delayed monsoon season
    ## keep C and E
sort(unique(north_edge$Plot))
sort(unique(sev_edge$plot))
sort(unique(north_edge$Subplot))
unique(sev_edge$subplot) ## nothing in this one
unique(sev_edge$quad) ## this seems like it could be equivalent to the northern edge subplots?
unique(sev_edge$site)
min(sev_edge$cover)
min(north_edge$max.cover)

sort(unique(sev_edge$date))
## based on what I've been hearing both 2012 and 2013 years were pre-treatment but 2012 had a big drought so maybe not the best data to use? (this is true at Northern sites but not at the SEV)

## Desired Changes ####
## list desired changes
## SEV - the site name should be changed from 'EDGE_black' or 'EDGE_blue' to SEV_black or SEV_blue
## SEV cover should be converted to max cover
## Consistent way of handling species names (4 letter code? make sure there's a key for everything)

## need to harmonize treatment colname and abbreviations
## consistent way of handling drought treatment codes

# Clean ####
## SEV ####
sort(unique(sev_edge$kartez))
sort(unique(sev_edge$genus))

empty <- sev_edge %>%
  filter(kartez == "EMPTY")
none <- sev_edge %>%
  filter(kartez == "NONE") ## this is actually a species, don't filter it out!!

## each of the unknowns shows up once
unk1 <- sev_edge %>%
  filter(kartez == "UNKFORB1")
unk2 <- sev_edge %>%
  filter(kartez == "UNKNOWN")

### sp questions
## should we keep species with 'NA' for specific epithet? - leave in
## what should we do with unknowns? - remove

rm_kartez <- c("EMPTY", "UNKFORB1", "UNKNOWN") ## remove unknowns and empty

### quantify unknowns ####
sev_unknowns = sev_edge %>%
  filter(kartez %in% rm_kartez,
         kartez != "EMPTY", treatment != "D")

### make mods ####
unique(sev_edge$year)
## 2012 = pre-treatment year
## 2013, 2014, 2015, 2016 = drought years
## Q HERE ####
## do I calculate the max cover by comparing spring and fall of diff or the same years?
## from readings, seems like growing season is ~Apr - Oct so it should be same year 
sev_clean <- sev_edge %>%
  mutate(site = ifelse(site == "EDGE_black", "SBK", "SBL"), ## fix site code
         subplot = quad, ## rename quad as subplot to match north sites
         spcode = paste0(substr(genus, 1, 3), substr(sp.epithet, 1, 3)), ## make 6 letter sp codes
         species = paste0(genus, "_", sp.epithet)) %>%
  mutate(across(c(spcode), toupper), ## capitalize
         kartez = ifelse(species == "Glandularia_bipinnatifida", "GLBI2", kartez)) %>% ## fix a kartez code to prevent row from duplicating
  mutate(experiment.year = year - 2012, 
         treatment.year = ifelse(year == 2012, "pre-treatment", 
                                 ifelse((2012 < year) & (year < 2017), "drought", "recovery"))) %>%
  filter(treatment != "D", !kartez %in% rm_kartez) %>% ## remove monsoon timing treatment and unknowns/empty species
  #pivot_wider(names_from = season, values_from = cover, values_fill = 0) %>%
  group_by(site, treatment, block, plot, subplot, year, experiment.year, treatment.year, species, spcode, kartez) %>% ## grouping by everything except season; this lets us take the maximum value of the season in the same calendar year.
  summarise(max.cover = max(cover)) %>%
  mutate(treatment = ifelse(treatment == "E", "D", treatment)) %>% ## change from E -> D for more intuitive notation
  ungroup() %>%
  group_by(site, treatment, block, plot, year, experiment.year, treatment.year, species, spcode, kartez) %>%
  summarise(mean.plot.cover = mean(max.cover)) %>% ## take mean cover of all 4 subplots in a plot- subplots are psuedoreplicated
  ungroup() %>%
  group_by(site, treatment, block, plot, year, experiment.year, treatment.year) %>%
  mutate(total.plot.cover = sum(mean.plot.cover),
         relative.sp.cover = mean.plot.cover/total.plot.cover) %>%
  select(year, site, treatment, block, plot, spcode, species, kartez, mean.plot.cover, experiment.year, treatment.year, relative.sp.cover, total.plot.cover)

colnames(sev_clean)

### duplicated row ####
duplicates <- sev_clean %>%
  group_by(site, year, block, plot, treatment, species) %>%
  filter(site == "SBL") %>%
  summarise(num.obs = n()) %>%
  filter(num.obs != 1)

blue_3_20_2019 <- sev_clean %>%
  filter(block == 3, plot == 20, year == 2019)
## Glandularia_bipinnatifida has 2 different kartez values, so grouping by this variable means we retain 2 rows for this species. 
## fixed now, should remain empty!

### explore species ####
summary <- sev_clean %>%
  group_by(site, treatment) %>%
  summarise(treat_richness = length(unique(species)))

summary2 <- sev_clean %>%
  group_by(site) %>%
  summarise(total_richness = length(unique(species)))

## North EDGE ####
colnames(north_edge)
sort(unique(north_edge$Species))
unique(north_edge$Spcode)

### sp notes ####
## LOTS of unknowns
## ulmus should probably be removed...?

sort(unique(north_edge$Species))

## remove unknowns
rm <- c("oxytopis_like_legume", "seedling_unknown", "UK_Fuzzy_Aster", "UK_onagraceac", "UK_poa", "UK_Tall_Phlox", "unk_Alien", "unk_alternate_leaf_forb", "unk_alternate_strong_midvein_hairy_margin", "unk_Aristida", "unk_Artemisia_ludoviciana", "UNK_Aster_rosette", "unk_astragalus_oxytropis", "unk_Clover", "unk_Eriogonum_Hays", "unk_fall_opposite_leaf", "unk_forb_soft_velvet", "unk_juicy_forb", "unk_Lepidium_like_forb", "unk_Milky_waxy", "unk_Oenothera", "unk_oenotheria", "unk_Oerothera_rosette", "unk_opposite_leaf", "Unk_overlapping_alt", "unk_Oxytropis_sp.", "unk_Primrose_like", "unk_Red_edged_forb", "unk_rush_unknown", "unk_Sonchus_seedling", "unk_Stipa_veridas", "unk_Tall_astragulus", "unk_Three_Leaf_Unknown_forb", "unk_Townsendia_grandiflora", "UNKFCHY1", "UNKFCHY2", "UNKFCHY3", "UNKFCHY4", "UNKFCHY5", "UNKFCHY6", "UNKFCHY7", "UNKFCHY8", "UNKFHYS1", "UNKFHYS2", "UNKFHYS3", "UNKFHYS4","UNKFHYS5", "UNKFHYS7", "UNKFHYS8", "UNKFKNZ1", "UNKFKNZ2", "UNKFKNZ3",  "unkforb_opp_Lvs", "UNKFSGS1", "UNKFSGS2", "UNKFSGS3", "UNKGRHYS1", "UNKGRHYS2","UNKHYS", "unknown", "Unknown_Cirsium", "Unknown_dry_sad", "Unknown_ericoides_small", "Unknown_Erysimum", "unknown_forb", "Unknown_forb", "unknown_forb_tooth", "Unknown_grass", "Unknown_linear_lvs", "unknown_machearanthera", "Unknown_milky_waxy", "Unknown_pilos_forb", "unknown_pinnately_lobed", "Unknown_ranunculus", "Unknown_rosette", "Unknown_Seedling", "unknown_shiny_alternate", "unknown_short_alternate", "Unknown_whorled_linear", "Unknown_woody", "UNKTRKNZ1", "UNKTRKNZ2", "huge_penstemon", "blob_unknown", "Ulmus_sp.", "NA_NA", "Ulmus_americana")

### quantify unknowns ####
north_unknowns = north_edge %>%
  filter(Species %in% rm, 
         max.cover > 0,
         Species != "Ulmus_americana",
         Species != "Ulmus_sp.", 
         Year > 2012,
         Trt != "int") %>%
  mutate(Site = fct_relevel(Site, "KNZ", "HYS", "CHY", "SGS"),
         cov_below1 = ifelse(max.cover <= 2, 1, 0))

sum(north_unknowns$cov_below1)/nrow(north_unknowns)
## 0.9375

length(north_unknowns$Site)
## 144

length(unique(north_unknowns$Species))
## 57

ggplot(north_unknowns, aes(x=max.cover)) +
  geom_histogram() +
  facet_wrap(~Site, ncol = 4, nrow = 1) +
  xlab("Unknown Species Cover") +
  ylab("Count")

ggsave("preliminary_figs/june_2024/north_sites_unknowns.png", width = 6, height = 2.5)
length(unique(north_unknowns$Species))

### clean data ####
north_clean <- north_edge %>%
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
         spcode = paste0(genuscode, spepcode)) %>%
  filter(treatment != "int", year > 2012, !species %in% rm) %>% ## remove 2012 as was drought pre-treat year
  mutate(treatment = ifelse(treatment == "chr", "D", "C"), 
         experiment.year = year - 2013, ## 2013 is pre-treat year
         treatment.year = ifelse(year == 2013, "pre-treatment", 
                                 ifelse((2013 < year) & (year < 2018), "drought", "recovery"))) %>%
  group_by(site, treatment, block, plot, year, species, spcode, kartez, experiment.year, treatment.year) %>%
  summarise(mean.plot.cover = mean(max.cover)) %>% ## take mean cover of all 4 subplots in a plot- subplots are psuedoreplicated
  ungroup() %>%
  group_by(site, treatment, block, plot, year, experiment.year, treatment.year) %>%
  mutate(total.plot.cover = sum(mean.plot.cover),
         relative.sp.cover = mean.plot.cover/total.plot.cover) %>%
  select(year, site, treatment, block, plot, spcode, species, kartez, mean.plot.cover, experiment.year, treatment.year, relative.sp.cover, total.plot.cover)

### finish quantifying unknowns ####
north_knowns = north_clean %>%
  filter(mean.plot.cover>0)

length(north_knowns$year)+length(north_unknowns$Spcode)

### explore species ####
summary <- north_clean %>%
  group_by(site, treatment) %>%
  summarise(treat_richness = length(unique(species)))

summary2 <- north_clean %>%
  group_by(site) %>%
  summarise(total_richness = length(unique(species)))

## Merge ####
colnames(north_clean) 
colnames(sev_clean)
edge_all <- rbind(north_clean, sev_clean)

unique(edge_all$site)
sort(unique(edge_all$year))

## Fill 0's ####
sev_black <- edge_all %>%
  filter(site == "SBK") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "mean.plot.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_eriopoda:Psilostrophe_tagetina, names_to = "species", values_to = "mean.plot.cover")

sev_blue <- edge_all %>%
  filter(site == "SBL") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "mean.plot.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_eriopoda:Ipomoea_costellata, names_to = "species", values_to = "mean.plot.cover")

hay <- edge_all %>%
  filter(site == "HYS") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "mean.plot.cover", values_fill = 0) %>%
  pivot_longer(Achillea_millefolium:Solanum_rostratum, names_to = "species", values_to = "mean.plot.cover")

knz <- edge_all %>%
  filter(site == "KNZ") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "mean.plot.cover", values_fill = 0) %>%
  pivot_longer(Ambrosia_psilostachya:Desmodium_illinoense, names_to = "species", values_to = "mean.plot.cover")

chy <- edge_all %>%
  filter(site == "CHY") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "mean.plot.cover", values_fill = 0) %>%
  pivot_longer(Allium_textile:Festuca_unknown, names_to = "species", values_to = "mean.plot.cover")

sgs <- edge_all %>%
  filter(site == "SGS") %>%
  ungroup() %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "mean.plot.cover", values_fill = 0) %>%
  pivot_longer(ASOX:Astragalus_fluxuosus, names_to = "species", values_to = "mean.plot.cover")

## merge all together
edge_w_zeros <- do.call("rbind", list(sev_black, sev_blue, hay, knz, chy, sgs)) %>%
  #group_by(site, block, plot, year, species) %>%
  #summarise(mean.plot.cover = mean(max.cover)) %>%
  mutate(pres.abs = ifelse(mean.plot.cover > 0, 1, 0))
  

## clean up env
rm(list = c("north_clean", "north_edge", "sev_clean", "sev_edge", "north_spkey", "empty", "none",  "unk1", "unk2", "rm_kartez", "blocks", "chy", "hay", "knz", "sev_black", "sev_blue", "sgs", "summary", "summary2", "blue_3_20_2019", "duplicates", "rm"))
