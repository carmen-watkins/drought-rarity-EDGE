## Clean LTER data to extract needed functional groups

## read in functional group data
lterFG <- read.csv("data/LTER-grasslands-master-2012.csv")
FG <- read.csv("data/Taxa_Info.csv")

## read in cleaned edge data
#source("data-prep/clean_edge_data.R")

sev_edge <- read.csv("data/sev298_NPP_edge_biomass.csv")


## Current progress
## functional group data is coming from 3 sources 
## each source has diff col names adn formatting and some differences in values (i.e. grass v graminoid)
## will take a big of cleaning up to make this the most useful

# Create ONE species key ####
## Create SEV species key ####
sev <- sev_edge %>%
  group_by(kartez) %>%
  select(genus, sp.epithet, kartez, FunctionalGroup, LifeHistory, PhotoPath)%>%
  distinct() %>%
  filter(!kartez %in% c("UNKFORB1", "UNKNOWN"))

sevkey <- sev %>%
  group_by(genus) %>%
  select(genus, FunctionalGroup) %>%
  distinct() %>%
  mutate(genus = tolower(genus)) %>%
  filter(!genus %in% c("gutierrezia"), ## gutierrezia has rows with FG forb and shrub - should manually look at species and see if there are two species
         !is.na(genus))

#unique(sevkey$genus)
#test <- sevkey %>%
 # filter(is.na(FunctionalGroup))

## northern sites FG key ####
FG2 <- FG %>%
  mutate(genuscode = toupper(substr(Genus, 1, 3)), ## make a species code column
         spepcode = toupper(substr(Species, 1, 3)),
         spcode = paste0(genuscode, spepcode)) %>%
  select(-Plant.code, genuscode, -spepcode) %>%
  filter(!spcode %in% c("UNKUNK", "UNKNA")) %>%
  distinct()
## CHASER not a distinct species code

FGkey <- FG2 %>%
  group_by(Genus) %>%
  select(Genus, Funct.grp) %>%
  distinct() %>%
  mutate(genus = tolower(Genus),
         FunctionalGroup = Funct.grp) %>%
  ungroup() %>%
  select(-Funct.grp, -Genus) %>%
  filter(!genus %in% c("gutierrezia"), ## gutierrezia has rows with FG forb and shrub - should manually look at species and see if there are two species
         !is.na(genus))

unique(FGkey$genus)

## LTER sites FG key ####
## keep only FG data from lter sites
lterFG2 <- lterFG %>%
  mutate(genuscode = toupper(substr(species, 1, 3)), ## make a species code column
         sp.ep = strsplit(species, " ") %>%
           sapply(tail, 1),
         spepcode = toupper(substr(sp.ep, 1, 3)),
         spcode = paste0(genuscode, spepcode)) %>%
  select(-sitesubplot, -year, -experiment, - site, - plot, -subplot, -subplota, -abundance, -unitAbund, -genuscode, -sp.ep, -spepcode, -location, -oldSpecies, -durationDetail, -growthDetail, -family) %>% ## drop unneeded columns
  distinct() #%>% ## keep distinct observations
  #filter(!spcode %in% c("ALLSP", "ARILON", "BRASP", "BRDBRD", "CHASER", "ASCVIR", "ASTSP", "ELYELY", "ERIANN", "HESSPA", "LAPOCC", "LUMLUM", "MIDMID", "SOLSP", "VIOPED", "CHASP", "HOMHOM", "BROSP"))

## create a key at the genus level
lterkey <- lterFG2 %>%
  mutate(genus = tolower(strsplit(species, " ")%>%
                           sapply(head, 1)),
         sp.ep = strsplit(species, " ")%>%
           sapply(tail, 1),
         FunctionalGroup = growth) %>%
  group_by(genus) %>%
  select(genus, FunctionalGroup)%>%
  distinct() %>%
  filter(FunctionalGroup != "", 
         genus != "polygonum") %>%
  mutate(FunctionalGroup = ifelse(FunctionalGroup == "forb/herb", "forb", ifelse(FunctionalGroup == "graminoid", "grass", FunctionalGroup)))

unique(lterkey$genus)
duplicated(lterkey$genus)

#lterkey[89,]

#test <- lterkey %>%
 # filter(FunctionalGroup == "vine")

## Master Key ####
genuskey <- rbind(sevkey, FGkey, lterkey) %>%
  distinct() %>%
  filter(!(genus == "carex" & FunctionalGroup == "sedge"),
         !(genus == "ipomoea" & FunctionalGroup == "vine"))

duplicated(genuskey$genus)
#genuskey[97,]
#genuskey[189,]

#gktest <- genuskey %>%
 # filter(genus %in% c("carex", "ipomoea"))

rm(lterkey, sevkey, FGkey, FG, FG2, sev, sev_edge, lterFG, lterFG2)
