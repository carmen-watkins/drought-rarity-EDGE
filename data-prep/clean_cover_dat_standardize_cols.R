# Header #### 
## Script name: Clean cover data, standardize cols
##
##' Purpose of script: Standardize cover data column names & site codes, 
##' select appropriate years and filter out monsoon timing treatment
##
## Author: Carmen Watkins
##

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
sev_edge <- read.csv("data/sev_edge_sites/sev298_NPP_edge_biomass.csv")
## the version of SEV data in the sev_download folder was downloaded on 11/5/2024 from EDI 
## (https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sev.298.209677)


# Clean ####
## SEV Sites ####
# unique(sev_edge$year)
    ## 2012 = pre-treatment year
    ## according to meta-data drought was applied 2013-2019
sev_temp <- sev_edge %>%
  mutate(site = ifelse(site == "EDGE_black", "SBK", "SBL"), ## fix site code
         subplot = quad, ## rename quad as subplot to match north sites
         spcode = paste0(substr(genus, 1, 3), substr(sp.epithet, 1, 3)), ## make 6 letter sp codes
         species = paste0(genus, "_", sp.epithet)) %>%
  mutate(experiment.year = year - 2012, 
         treatment.year = ifelse(year == 2012, "pre-treatment", 
                                 ifelse((2012 < year) & (year < 2020), 
                                        "drought", "recovery"))) %>%
  filter(treatment != "D") %>% ## remove monsoon timing treatment 
  mutate(across(c(spcode), toupper)) %>%
  select(-date, -web, -transect, -quad, -family, -LifeHistory, -PhotoPath,
         -FunctionalGroup, -volume, -biomass.BM, -biomass.BIM, -SiteCluster,
         -MetStation, -season.precip, -GDD, -SPEI.comp)

## North Sites ####
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
  ## remove monsoon timing treatment 
  ## remove 2012 as was drought pre-treat year
  filter(treatment != "int") %>%  #, year > 2012
  ## set treatment years
  mutate(treatment = ifelse(treatment == "chr", "D", "C"), 
         experiment.year = year - 2013, ## 2013 is pre-treat year
         treatment.year = ifelse(year == 2013, "pre-treatment", 
                                 ifelse((2013 < year) & (year < 2018),
                                        "drought", "recovery"))) %>%
  mutate(across(c(spcode), toupper)) %>%
  select(-kartez, -Spcode, -Species, -spepcode, -genuscode, -Site, -Plot, 
         -Block, -Trt, -Subplot, -Year)

# Split ####
## split by sites to clean species names individually
sbk = sev_temp %>%
  filter(site == "SBK")

sbl = sev_temp %>%
  filter(site == "SBL")

sgs = north_temp %>%
  filter(site == "SGS")

chy = north_temp %>%
  filter(site == "CHY")

hys = north_temp %>%
  filter(site == "HYS")

knz = north_temp %>%
  filter(site == "KNZ")

## clean up environment
rm(north_edge, north_spkey, north_temp, sev_edge, sev_temp)
