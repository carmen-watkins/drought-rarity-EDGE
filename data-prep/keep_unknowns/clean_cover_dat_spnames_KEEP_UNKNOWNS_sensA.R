# Header #### 
## Script name: Clean cover data species names
##
## Sensitivity analysis version - lump species but do not remove unknown observations
##
## Purpose of script: Clean species names at each site individually
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
source("data-prep/clean_cover_dat_standardize_cols.R")

# SEV Sites ####
## SBK #### 
sort(unique(sbk$species))
sort(unique(sbk$genus))
sort(unique(sbk$kartez)) 
## "UNKNOWN", "EMPTY" present, remove
sort(unique(sbk$sp.epithet))

## find species with genus id present, but no sp epithet id
sbk_unk_epithet = sbk %>%
  filter(is.na(sp.epithet) | sp.epithet %in% c("sp.", "sp. ", "Seedling", "seedling", "unknown", "sp", "small"))

unique(sbk_unk_epithet$genus)
## 3 genera with unidentified sp epithets
## "Sporobolus"  "Astragalus"  "Sphaeralcea"

astrag = sbk %>%
  filter(genus == "Astragalus") %>%
  group_by(site, species, year, block, plot) %>%
  summarise(num.obs = n())
## seems like astragalus was perhaps not identified by species until 2016
## lump "Astragalus_missouriensis", "Astragalus_NA", and "Astragalus_nuttallianus" all as "Astragalus_sp"
## kartez to ASTRA
table(astrag$species, astrag$plot)
table(astrag$species, astrag$year)

sphaer = sbk %>%
  filter(genus == "Sphaeralcea") %>%
  group_by(site, species, year, block, plot) %>%
  summarise(num.obs = n())
## only one observation of Sphaeralcea_NA in 2020
## found in the same plot as 2 species of Sphaeralcea
## remove this observation
table(sphaer$species, sphaer$plot)

sporob = sbk %>%
  filter(genus == "Sporobolus") %>%
  group_by(site, species, year, block, plot) %>%
  summarise(num.obs = n())
## lump at genus level; 
## "Sporobolus_contractus", "Sporobolus_cryptandrus", "Sporobolus_flexuosus", and "Sporobolus_NA" all as "Sporobolus_sp"
## kartez to SPORO
## can only determine species by reproductive structures, so if no seedheads in a year sp.epithet is marked as NA
table(sporob$species, sporob$plot)
table(sporob$species, sporob$year)

unique(sporob$species)

### clean sp ####
sbk_sp = sbk %>%
  
  
   filter( #species != "Sphaeralcea_NA",
         !kartez %in% c( #"UNKNOWN", 
                        "EMPTY")) %>% ## only get rid of the empty designation, keep the rest of the unknowns
  
  ## lump all astragalus species as Astragalus_sp
  mutate(spcode = ifelse(genus == "Astragalus", "ASTSP", spcode),
         species = ifelse(genus == "Astragalus", "Astragalus_sp", species),
         kartez = ifelse(genus == "Astragalus", "ASTRA", kartez), 
         
         ## lump all sporobolus species as Sporobolus_sp       
         spcode = ifelse(genus == "Sporobolus", "SPOSP", spcode),
         species = ifelse(genus == "Sporobolus", "Sporobolus_sp", species),
         kartez = ifelse(genus == "Sporobolus", "SPORO", kartez))

### quantify unknowns ####
sbk_unks = sbk %>%
  filter(species == "Sphaeralcea_NA" | kartez %in% c("UNKNOWN"))

unique(sbk_unks$species)
nrow(sbk_unks)

## SBL ####
sort(unique(sbl$species))
sort(unique(sbl$genus))
sort(unique(sbl$kartez)) 

## find species with genus id present, but no sp epithet id
sbl_unk_epithet = sbl %>%
  filter(is.na(sp.epithet) | sp.epithet %in% c("sp.", "sp. ", "Seedling", "seedling", "unknown", "sp", "small"))

unique(sbl_unk_epithet$genus)
## 2 genera with unidentified sp epithets
## "Sporobolus"  "Astragalus" 

astrag = sbl %>%
  filter(genus == "Astragalus") %>%
  group_by(site, species, year, block, plot) %>%
  summarise(num.obs = n())
## seems like astragalus was perhaps not identified by species until 2016
## lump "Astragalus_missouriensis", "Astragalus_NA", and "Astragalus_nuttallianus" all as "Astragalus_sp"
## kartez to ASTRA
table(astrag$species, astrag$year)
table(astrag$species, astrag$plot)

sporob = sbl %>%
  filter(genus == "Sporobolus") %>%
  group_by(site, species, year, block, plot) %>%
  summarise(num.obs = n())
## lump at genus level; 
## "Sporobolus_contractus", "Sporobolus_cryptandrus", "Sporobolus_flexuosus", and "Sporobolus_NA" all as "Sporobolus_sp"
## kartez to SPORO
## can only determine species by reproductive structures, so if no seedheads in a year sp.epithet is marked as NA
table(sporob$species, sporob$plot)
table(sporob$species, sporob$year)

glandularia = sbl %>%
  filter(genus == "Glandularia") %>%
  group_by(site, species, kartez, year, block, plot) %>%
  summarise(num.obs = n())
table(glandularia$kartez, glandularia$year)
## looks like GLWR shows up mainly in 2017 and once in 2019
table(glandularia$kartez, glandularia$plot)
## found mainly in plots with GLBI2 and a few times in plots without it
## will stick with lumping all as GLBI2

### clean sp ####
sbl_sp = sbl %>%
  
  ## lump all astragalus species as Astragalus_sp
  mutate(spcode = ifelse(genus == "Astragalus", "ASTSP", spcode),
         species = ifelse(genus == "Astragalus", "Astragalus_sp", species),
         kartez = ifelse(genus == "Astragalus", "ASTRA", kartez), 
         
         ## lump all sporobolus species as Sporobolus_sp       
         spcode = ifelse(genus == "Sporobolus", "SPOSP", spcode),
         species = ifelse(genus == "Sporobolus", "Sporobolus_sp", species),
         kartez = ifelse(genus == "Sporobolus", "SPORO", kartez),
         
         ## fix a kartez code to prevent row from duplicating
         ## two kartez codes for Glandularia in data: GLBI1 and GLWR
         kartez = ifelse(species == "Glandularia_bipinnatifida", "GLBI2", kartez))

# Northern Sites ####
## KNZ ####
sort(unique(knz$species))
sort(unique(knz$genus))
sort(unique(knz$sp.ep))

## to remove right away 
## remove trees + unknowns with no identifying genus info
knz_rm1 = c("blob_unknown", "Ulmus_americana", "Ulmus_sp.", "unk_rush_unknown", "UNKFKNZ1", "UNKFKNZ2", "Unknown_Seedling", "UNKTRKNZ1", "UNKTRKNZ2")

knz_temp = knz #%>%
  #filter(!species %in% knz_rm1)

## find species with genus id present, but no sp epithet id
knz_unk_epithet = knz_temp %>%
  filter(is.na(sp.ep) | sp.ep %in% c("sp.", "sp. ", "Seedling", "seedling", "unknown", "sp", "small"))

unique(knz_unk_epithet$genus)

amaran = knz %>%
  filter(genus == "Amaranthus") %>% 
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(amaran$site, amaran$species)
## just one obs; okay to leave as species

cirsium = knz %>%
  filter(genus %in% c("Circium", "Cirsium")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(cirsium$site, cirsium$species)
## just 2 observations; lump these together
table(cirsium$species, cirsium$year)
table(cirsium$species, cirsium$plot)

eleoch = knz %>%
  filter(genus %in% c("Eleocharis")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(eleoch$species, eleoch$year)
table(eleoch$species, eleoch$plot)
## only Eleocharis_sp obs; leave as is

euphorb = knz %>%
  filter(genus %in% c("Euphorbia")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(euphorb$species, euphorb$year)
table(euphorb$species, euphorb$plot)
## lump all as euphorbia species since majority are already lumped

panic = knz %>%
  filter(genus %in% c("panicum", "Panicum")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(panic$species, panic$year)
table(panic$species, panic$plot)
## remove panicum_unknown as there are only 2 observations but two other species are ID'ed to species level

unks = knz_temp %>%
  filter(genus %in% c("Unknown", "unk"))
table(unks$year, unks$species)
## unk_rush_unknown & Unknown_Seedling already marked for removal
## Unknown_ericoides_small should be removed, ericoides is not a genus

### clean sp ####
knz_sp = knz_temp %>%
  #filter(!species %in% c("panicum_unknown", "Unknown_ericoides_small")) %>%
  
  ## lump cirsium
  mutate(spcode = ifelse(genus %in% c("Circium", "Cirsium"), "CIRSP", spcode),
         species = ifelse(genus %in% c("Circium", "Cirsium"), "Cirsium_sp", species),
         
         ## lump euphorbia species
         #spcode = ifelse(genus %in% c("Euphorbia"), "EUPSP", spcode),
        # species = ifelse(genus %in% c("Euphorbia"), "Euphorbia_sp", species),
         
        ## Based on feedback from Mendy on 1/29/2025, don't lump Euphorbia sp at KNZ; Euphorbia marginata is distinct; Euphorbia sp is probably 2 sp
        
         ## change ASSY to Asclepias syriaca
         species = ifelse(species == "ASSY", "Asclepias_syriaca", species),
         genus = ifelse(genus == "ASSY", "Asclepias", genus),
         sp.ep = ifelse(sp.ep == "ASSY", "syriaca", sp.ep))

sort(unique(knz_sp$species))
sort(unique(knz_sp$genus))
sort(unique(knz_sp$sp.ep))

### quantify unknowns ####
knz_unks = knz %>%
  filter(species %in% knz_rm1 | species %in% c("panicum_unknown", "Unknown_ericoides_small"),
         !species %in% c("Ulmus_americana", "Ulmus_sp."))

length(unique(knz_unks$species))
nrow(knz_unks)

## HYS ####
sort(unique(hys$species))
## to remove right away 
## remove trees + unknowns with no identifying genus info
hys_rm1 = c("oxytopis_like_legume", "seedling_unknown", "Ulmus_sp.", "unk_alternate_leaf_forb",      "unk_fall_opposite_leaf", "unk_juicy_forb", "unk_mustard_unknown", "Unk_overlapping_alt", "UNKFHYS4", "UNKFHYS5", "UNKFHYS7", "UNKFHYS8", "unkforb_opp_Lvs", "UNKGRHYS1", "UNKHYS", "unknown_forb", "Unknown_rosette", "Unknown_woody")

hys_temp = hys #%>%
#  filter(!species %in% hys_rm1)

sort(unique(hys_temp$genus))
sort(unique(hys_temp$sp.ep))

## find species with genus id present, but no sp epithet id
hys_unk_epithet = hys_temp %>%
  filter(is.na(sp.ep) | sp.ep %in% c("sp.", "sp. ", "Seedling", "seedling", "unknown", "sp", "small", "Hays"))

unique(hys_unk_epithet$genus)
## "Asclepias"   "Chenopodium" "Chloris" "Euphorbia" "Melilotus" "unk" "Triodanis" "Astragalus" "Croton"  

asclep = hys_temp %>%
  filter(genus == "Asclepias") %>% 
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(asclep$species, asclep$year)
table(asclep$species, asclep$plot)
### DECISION HERE ####
## lump all or remove Asclepias_sp. observations? 
## removing unknowns for now
## REMOVE

ggplot(asclep, aes(x=year, y=plot, color = species)) +
  geom_point() +
  facet_wrap(~species, nrow = 1, ncol = 6)

virid_sp = asclep %>%
  filter(species %in% c("Asclepias_sp.", "Asclepias_viridis"))

ggplot(virid_sp, aes(x=year, y=plot, color = species)) +
  geom_jitter()

table(virid_sp$plot, virid_sp$species)


chenop = hys_temp %>%
  filter(genus %in% c("Chenopodium")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(chenop$year, chenop$species)
table(chenop$species, chenop$plot)
## already lumped at genus level

chloris = hys_temp %>%
  filter(genus %in% c("Chloris")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(chloris$year, chloris$species)
## already lumped at genus level

euphorb = hys_temp %>%
  filter(genus %in% c("Euphorbia", "Euphorbiadavidii")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(euphorb$species, euphorb$year)
table(euphorb$species, euphorb$plot)
## lump all as euphorbia species since majority are already lumped

melilo = hys_temp %>%
  filter(genus %in% c("Melilotus")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(melilo$species, melilo$year)
table(melilo$species, melilo$plot)

## already lumped at genus level

triodan = hys_temp %>%
  filter(genus %in% c("Triodanis")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(triodan$species, triodan$year)
## already lumped at genus level

croton = hys_temp %>%
  filter(genus %in% c("Croton")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(croton$species, croton$year)
## already lumped at genus level

astrag = hys_temp %>%
  filter(genus %in% c("Astragalus")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(astrag$species, astrag$year)
table(astrag$species, astrag$plot)
## mostly Astragalus_sp, one Astragalus_unknown, some split out to a speicfic species; lump all of these

unks = hys_temp %>%
  filter(genus %in% c("Unknown", "unk")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(unks$species, unks$year)
table(unks$species, unks$plot)

## unk_Eriogonum_Hays unk_Oenothera unk_oenotheria Unknown_Cirsium

eriogo = hys_temp %>%
  filter(genus %in% c("Eriogonum")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(eriogo$species, eriogo$year)
table(eriogo$species, eriogo$plot)
## only one eriogonum observation otherwise; lump the unk_Eriogonum_Hays into this

oenoth = hys_temp %>%
  filter(genus %in% c("Oenothera")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(oenoth$species, oenoth$year)
table(oenoth$species, oenoth$plot)
## only one Oenothera species at HYS - could lump the two unknowns into this one? 

unkO = unks %>%
  filter(species %in% c("unk_Oenothera", "unk_oenotheria"))
table(unkO$year, unkO$plot)
table(unkO$species, unkO$year)
## one of the unknowns shows up in the same plot as an Oenothera_suffratescens

### DECISION HERE ####
## lump for now? but run by someone else
## could also remove
## remove

cirsium = hys_temp %>%
  filter(genus %in% c("Circium", "Cirsium")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(cirsium$year, cirsium$species)
table(cirsium$year, cirsium$plot)
## only one cirsium species at the site

unkC = unks %>%
  filter(species == "Unknown_Cirsium")
table(unkC$year, unkC$plot)

## the one unknown Cirsium was found in 2021, when multiple C. undulatum were found as well. 
## the unknown was found in a different plot than other observed C. undulatum individuals.

## remove the unknown


### clean sp ####
hys_sp = hys_temp %>%
#  filter(!species %in% c("Unknown_Cirsium", "Asclepias_seedling", "Asclepias_sp.")) %>%
  
  ## lump euphorbia species
  mutate(## spcode = ifelse(genus %in% c("Euphorbia", "Euphorbiadavidii"), "EUPSP", spcode),
         ## species = ifelse(genus %in% c("Euphorbia", "Euphorbiadavidii"), "Euphorbia_sp", species),
    ##keep euphorbia separate based on advice from Mendy 1/29/25
    
         ## lump astragalus species 
         spcode = ifelse(genus %in% c("Astragalus"), "ASTSP", spcode),
         species = ifelse(genus %in% c("Astragalus"), "Astragalus_sp", species),
         
         ## lump eriogonum species
         genus = ifelse(species %in% c("unk_Eriogonum_Hays", "Eriogonum_effusum"), "Eriogonum", genus),
         spcode = ifelse(species %in% c("unk_Eriogonum_Hays", "Eriogonum_effusum"), "ERISP", spcode),
         species = ifelse(species %in% c("unk_Eriogonum_Hays", "Eriogonum_effusum"), "Eriogonum_sp", species),
         
         ## lump oenothera
         genus = ifelse(species %in% c("unk_Oenothera", "unk_oenotheria", "Oenothera_suffrutescens"), "Oenothera", genus),
         spcode = ifelse(species %in% c("unk_Oenothera", "unk_oenotheria", "Oenothera_suffrutescens"), "OENSP", spcode),
         species = ifelse(species %in% c("unk_Oenothera", "unk_oenotheria", "Oenothera_suffrutescens"), "Oenothera_sp", species)
         
  )

sort(unique(hys_sp$species))
sort(unique(hys_sp$genus))
sort(unique(hys_sp$sp.ep))

#check = hys_sp %>%
#filter(sp.ep == "unknown")
## the unknown species is a lumped Astragalus, all ok

### quantify unknowns ####
hys_unks = hys %>%
  filter(species %in% hys_rm1 | species %in% c("Unknown_Cirsium", "Asclepias_seedling", "Asclepias_sp."),
         !species %in% c("Ulmus_americana", "Ulmus_sp."))

length(unique(hys_unks$species))
nrow(hys_unks)

## CHY ####
sort(unique(chy$species))
## to remove right away 
## remove trees + unknowns with no identifying genus info
chy_rm1 = c("UK_Fuzzy_Aster", "UK_onagraceac", "UK_Tall_Phlox", "unk_alternate_strong_midvein_hairy_margin", "UNK_Aster_rosette", "UNKFCHY1", "UNKFCHY3", "UNKFCHY4", "UNKFCHY5", "UNKFCHY6", "UNKFCHY7", "unknown", "Unknown_forb", "Unknown_linear_lvs", "Unknown_rosette", "unknown_shiny_alternate")

chy_temp = chy #%>%
#  filter(!species %in% chy_rm1)

sort(unique(chy_temp$genus))
sort(unique(chy_temp$sp.ep))

## find species with genus id present, but no sp epithet id
chy_unk_epithet = chy_temp %>%
  filter(is.na(sp.ep) | sp.ep %in% c("sp.", "sp. ", "Seedling", "seedling", "unknown", "sp", "small", "Hays"))

sort(unique(chy_unk_epithet$genus))
## "Astragalus"  "Chenopodium" "Festuca"     "Oenothera"   "Orobanche"   "Paronychia"  "Silene"      "Sporobolus"  "unk"  

astrag = chy_temp %>%
  filter(genus %in% c("Astragalus")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(astrag$species, astrag$year)
## Astragalus_sp is least prevalent, remove

ggplot(astrag, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_wrap(~species)
## remove Astragalus_sp

chenop = chy_temp %>%
  filter(genus %in% c("Chenopodium")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(chenop$species, chenop$year)
## already lumped at genus level

festuca = chy_temp %>%
  filter(genus %in% c("Festuca")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(festuca$year, festuca$species)
## only one observation, can leave as is

oenoth = chy_temp %>%
  filter(genus %in% c("Oenothera")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(oenoth$species, oenoth$year)

ggplot(oenoth, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_wrap(~species)
## remove Oenothera_sp.

oroban = chy_temp %>%
  filter(genus %in% c("Orobanche")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(oroban$year, oroban$species)
## already lumped at genus level

paron = chy_temp %>%
  filter(genus %in% c("Paronychia")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(paron$year, paron$species)
## already lumped at genus level

silene = chy_temp %>%
  filter(genus %in% c("Silene")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(silene$year, silene$species)
## already lumped at genus level

sporob = chy_temp %>%
  filter(genus %in% c("Sporobolus")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(sporob$year, sporob$species)
## already lumped at genus level

unks = chy_temp %>%
  filter(genus %in% c("Unknown", "unk"))
table(unks$year, unks$species)
## unk_Artemisia_ludoviciana unk_astragalus_oxytropis unk_Oxytropis_sp. unk_Stipa_veridas Unknown_Erysimum

artemesia = chy_temp %>%
  filter(genus %in% c("Artemesia", "Artemisia")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(artemesia$year, artemesia$species)
## no Artemisia_ludoviciana at all; remove 'unk_Artemisia_ludoviciana'

astrag_oxy = chy_temp %>%
  filter(genus %in% c("Astragalus", "Oxytropis")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(astrag_oxy$year, astrag_oxy$species)

table(astrag_oxy$year, astrag_oxy$plot)

ggplot(astrag_oxy, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3)+
  geom_hline(yintercept = 17) +
  facet_wrap(~species) +
  geom_vline(xintercept = 2018) +
  geom_vline(xintercept = 2019) +
  geom_vline(xintercept = 2021)

unkASOX = unks %>%
  filter(species %in% c("unk_astragalus_oxytropis", "unk_Oxytropis_sp.")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(unkASOX$year, unkASOX$species)
table(unkASOX$year, unkASOX$plot)

ggplot(unkASOX, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3)

## the unk_astragalus_oxytropis and unk_Oxytropis_sp. really seems like they could be the same thing as Oxytropis_lambertii in plot 17, which was found 2016 & 2017; unk_astragalus_oxytropis was found in same plot in 2018 & 2019; unk_oxytropis_sp was found in same plot in 2021...
## remove for now, but consider lumping
## final decision; remove!!

stipa = chy_temp %>%
  filter(genus %in% c("Nassella", "Stipa")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(stipa$year, stipa$species)
table(stipa$year, stipa$plot)

ggplot(stipa, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_wrap(~species) 

unkSt = unks %>%
  filter(species == "unk_Stipa_veridas") %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(unkSt$year, unkSt$plot)
## remove unk_Stipa_veridas, not in the same plot as Nassella viridula

erysim = chy_temp %>%
  filter(genus %in% c("Erysimum")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(erysim$year, erysim$species)
## no sp in this genus here
## remove Unknown_Erysimum

## "huge_penstemon"   
penstem = chy_temp %>%
  filter(genus %in% c("Penstemon", "huge")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(penstem$species, penstem$year)
table(penstem$species, penstem$plot)
## only one penstemon species

ggplot(penstem, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3)
## lump this into Penstemon_albidus

### clean sp ####
chy_sp = chy_temp %>%
 # filter(!species %in% c("Astragalus_sp.", "unk_astragalus_oxytropis", "unk_Oxytropis_sp.",
                     #    "Festuca_unknown",
                      #   "Oenothera_sp.",
                       #  "unk_Artemisia_ludoviciana",  "unk_Stipa_veridas", "Unknown_Erysimum")) %>%
  
  ## fix penstemon
  mutate(genus = ifelse(species %in% c("huge_penstemon"), "Penstemon", genus),
         spcode = ifelse(species %in% c("huge_penstemon"), "PENALB", spcode),
         species = ifelse(species %in% c("huge_penstemon"), "Penstemon_albidus", species))

sort(unique(chy_sp$species))
sort(unique(chy_sp$genus))
sort(unique(chy_sp$sp.ep))

### quantify unknowns ####
chy_unks = chy %>%
  filter(species %in% chy_rm1 | species %in% c("Astragalus_sp.", "unk_astragalus_oxytropis", "unk_Oxytropis_sp.", "Festuca_unknown", "Oenothera_sp.", "unk_Artemisia_ludoviciana",  "unk_Stipa_veridas", "Unknown_Erysimum"))

length(unique(chy_unks$species))
nrow(chy_unks)

## SGS ####
sort(unique(sgs$species))
## to remove right away 
## remove trees + unknowns with no identifying genus info
sgs_rm1 = c("unk_Alien", "unk_Lepidium_like_forb", "unk_Red_edged_forb", "UNKFSGS2", "UNKFSGS3", "Unknown_grass", "Unknown_milky_waxy")

sgs_temp = sgs #%>%
#  filter(!species %in% sgs_rm1)

sort(unique(sgs_temp$genus))
sort(unique(sgs_temp$sp.ep))

## find species with genus id present, but no sp epithet id
sgs_unk_epithet = sgs_temp %>%
  filter(is.na(sp.ep) | sp.ep %in% c("sp.", "sp. ", "Seedling", "seedling", "unknown", "sp", "small", "Hays", "ASOX", "astragalus", "unk") | genus %in% c("unk"))

sort(unique(sgs_unk_epithet$genus))
# "ASOX"        "Chenopodium" "Euphorbia"   "Oenothera"   "Orobanche"  "unk"

astrag = sgs_temp %>%
  filter(genus %in% c("Astragalus", "ASOX", "unk", "Oxytropis")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(astrag$species, astrag$year)

ggplot(astrag, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_wrap(~species)
## will have to lump all of these together

chenop = sgs_temp %>%
  filter(genus %in% c("Chenopodium")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(chenop$species, chenop$year)
## already lumped at genus level

euphorb = sgs_temp %>%
  filter(genus %in% c("Euphorbia")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(euphorb$species, euphorb$year)
## only one observation, can leave as is

oenoth = sgs_temp %>%
  filter(genus %in% c("Oenothera")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(oenoth$species, oenoth$year)
table(oenoth$species, oenoth$plot)

ggplot(oenoth, aes(x=year, y=plot, color = species)) +
  geom_point(size = 3) +
  facet_wrap(~species)
## seems like oenothera sp was eventually ID'ed as Oenothera_albicaulis, lump all together

oroban = sgs_temp %>%
  filter(genus %in% c("Orobanche")) %>%
  group_by(site, year, species, block, plot) %>%
  summarise(num.obs = n())
table(oroban$year, oroban$species)
## already lumped at genus level

unks = sgs_temp %>%
  filter(genus %in% c("Unknown", "unk"))
table(unks$year, unks$species)
## unk_Tall_astragulus
## already decided to lump this one with ASOX

### clean sp ####
sgs_sp = sgs_temp %>%
  
  ## lump astragalus and oxytropis
  mutate(spcode = ifelse(species %in% c("ASOX", "Astragalus_fluxuosus", "Astragalus_gracilis", "Astragalus_lotiflorus", "unk_Tall_astragulus"), "ASOX", spcode),
         
         genus = ifelse(species %in% c("ASOX", "Astragalus_fluxuosus", "Astragalus_gracilis", "Astragalus_lotiflorus", "unk_Tall_astragulus"), "Astragalus", genus),
         
         species = ifelse(species %in% c("ASOX", "Astragalus_fluxuosus", "Astragalus_gracilis", "Astragalus_lotiflorus", "unk_Tall_astragulus"), "Astragalus_sp", species),
         ## keep A. mollissimus, A. shortianus, oxytropis separate
         
         ## lump oenothera
         spcode = ifelse(genus %in% c("Oenothera"), "OENSP", spcode),
         species = ifelse(genus %in% c("Oenothera"), "Oenothera_sp", species))

sort(unique(sgs_sp$species))
sort(unique(sgs_sp$genus))
sort(unique(sgs_sp$sp.ep))

### quantify unknowns ####
sgs_unks = sgs %>%
  filter(species %in% sgs_rm1)

length(unique(sgs_unks$species))
nrow(sgs_unks)


# Clean Env ####
rm(amaran, artemesia, asclep, astrag, astrag_oxy, chenop, chloris, chy, chy_rm1, chy_temp, chy_unk_epithet, cirsium, croton, eleoch, eriogo, erysim, euphorb, festuca, glandularia, hys, hys_rm1, hys_temp, hys_unk_epithet, knz, knz_rm1, knz_unk_epithet, melilo, oenoth, oroban, panic, paron, penstem, sbk, sbk_unk_epithet, sbl, sbl_unk_epithet, sgs, sgs_rm1, sgs_temp, sgs_unk_epithet, silene, sphaer, sporob, stipa, triodan, unkASOX, unkC, unkO, unks, unkSt)
