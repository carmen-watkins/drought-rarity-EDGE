## Clean & Combine all EDGE sites

## load packages
library(tidyverse)


# Read in Data ####
## northern edge
north_edge <- read.csv("shape-shifting-subordinates/spcomp_subplot_names.csv")
    ## max cover combines the two seasonal samplings and takes the largest of these, I believe. 
    ## Don't think this has been done with the sev site yet

north_spkey <- read.csv("shape-shifting-subordinates/spnames_code.csv")

## sev edge 
sev_edge <- read.csv("shape-shifting-subordinates/sev298_NPP_edge_biomass.csv")
    ## there is cover data here also
    ## would be good to talk to SEV EDGE expert and confirm a few things about the data here


# Explore Data ####
colnames(north_edge)
colnames(north_spkey)
colnames(sev_edge)

## list desired changes
    ## SEV - the site name should be changed from 'EDGE_black' or 'EDGE_blue' to SEV_black or SEV_blue
    ## SEV cover should be converted to max cover
    ## Consistent way of handling species names (4 letter code? make sure there's a key for everything)

    ## need to harmonize treatment colname and abbreviations
    ## consistent way of handling drought treatment codes

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


sort(unique(sev_edge$date))
## based on what I've been hearing both 2012 and 2013 years were pre-treatment but 2012 had a big drought so maybe not the best data to use?

sev_clean <- sev_edge %>%
  mutate(site = ifelse(site == "EDGE_black", "SEV_black", "SEV_blue"),
         subplot = quad,
         spcode = paste0(substr(genus, 1, 3), substr(sp.epithet, 1, 3)),
         species = paste0(genus, "_", sp.epithet)) %>%
  mutate(across(c(spcode), toupper)) %>% ## capitalize
  filter(year > 2012, treatment != "D") %>%
  pivot_wider(names_from = season, values_from = cover, values_fill = 0) %>%
  group_by(site, year, block, plot, subplot, treatment, spcode, species, kartez) %>%
  summarise(across(c(fall, spring), max)) %>%
  mutate(max.cover = ifelse(fall > spring, fall, spring),
         treatment = ifelse(treatment == "E", "D", treatment)) %>% ## change from E -> D for more intuitive notation
  select(year, site, treatment, block, plot, subplot, spcode, species, kartez, max.cover)

colnames(sev_clean)
colnames(north_edge)

north_clean <- north_edge %>%
  mutate(site = Site,
         plot = Plot,
         block = Block, 
         treatment = Trt, 
         subplot = Subplot,
         species = Species,
         kartez = NA,
         year = Year,
         genuscode = toupper(substr(Species, 1, 3)), 
         sp.ep = strsplit(Species, "_") %>%
           sapply(tail, 1),
         spepcode = toupper(substr(sp.ep, 1, 3)),
         spcode = paste0(genuscode, spepcode)) %>%
  filter(treatment != "int") %>%
  mutate(treatment = ifelse(treatment == "chr", "D", "C")) %>%
  select(year, site, treatment, block, plot, subplot, spcode, species, kartez, max.cover)

colnames(north_clean) 
colnames(sev_clean)

edge_all <- rbind(north_clean, sev_clean)

unique(edge_all$site)


## clean up env
rm(list = c("north_clean", "north_edge", "sev_clean", "sev_edge", "north_spkey"))
