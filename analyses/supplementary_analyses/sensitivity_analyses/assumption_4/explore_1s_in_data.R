# Header ####
## Script name: Explore 1's in data 
##
## Purpose of script: explore species that are gained or lost (RR of 1 or -1)
## in data. This is just an exploratory script, does not end up in MS.
##
## Author: Carmen Watkins

# Set Up ####
source("analyses/supplementary_analyses/sensitivity_analyses/assumption_4/filter_1s.R")
SKsp = unique(SKdrop$species)

# SBK ####
names(edge_all)
sort(SKsp)

SBK = edge_all %>%
  filter(site == "SBK") %>%
  filter(species %in% c(SKsp))



## Machaeranthera_tanacetifolia; why is this species showing up as 1 and -1
#Krascheninnikovia_lanata
#Machaeranthera_canescens
# Larrea_tridentata





sbk_check2 = sbk_check %>%
  filter(resp.ratio.site_D4 %in% c(1, -1, NaN) & 
           resp.ratio.site_PDfull %in% c(1, -1, NaN))


checks = SBK %>%
  filter(species %in% c(unique(sbk_check2$species))) %>%
  filter(max.cover > 0)

one_obs_only = checks %>%
  group_by(species) %>%
  summarise(count = n())

DROP = one_obs_only[one_obs_only$count == 1,]$species

DROP2 = one_obs_only[one_obs_only$count == 2,]$species

SBK %>%
  filter(species == "Hoffmannseggia_glauca") %>%
  filter(max.cover > 0) %>%
  ggplot(aes(x=year, y=max.cover)) +
  geom_point() +
  facet_wrap(~plot)

SBK %>%
  filter(species == "Hymenopappus_filifolius") %>%
  filter(max.cover > 0) %>%
  ggplot(aes(x=year, y=max.cover)) +
  geom_point() +
  facet_wrap(~plot)

SBK %>%
  filter(species == "Lotus_plebeius") %>%
  filter(max.cover > 0) %>%
  ggplot(aes(x=year, y=max.cover)) +
  geom_point() +
  facet_wrap(~plot)

SBK %>%
  filter(species == "Machaeranthera_gracilis") %>%
  filter(max.cover > 0) %>%
  ggplot(aes(x=year, y=max.cover)) +
  geom_point() +
  facet_wrap(~plot)

SBK %>%
  filter(species == "Sphaeralcea_wrightii") %>%
  filter(max.cover > 0) %>%
  ggplot(aes(x=year, y=max.cover)) +
  geom_point() +
  facet_wrap(~plot)

SBK %>%
  filter(species == "Machaeranthera_tanacetifolia") %>%
  filter(max.cover > 0) %>%
  ggplot(aes(x=year, y=max.cover, color = treatment)) +
  geom_point() +
  facet_wrap(~plot)

SBK %>%
  filter(species == "Machaeranthera_canescens") %>%
  filter(max.cover > 0) %>%
  ggplot(aes(x=year, y=max.cover, color = treatment)) +
  geom_point() +
  facet_wrap(~plot)

SBK %>%
  filter(species == "Larrea_tridentata") %>%
  filter(max.cover > 0) %>%
  ggplot(aes(x=year, y=max.cover, color = treatment)) +
  geom_point() +
  facet_wrap(~plot)

SBK %>%
  filter(species == "Krascheninnikovia_lanata") %>%
  filter(max.cover > 0) %>%
  ggplot(aes(x=year, y=max.cover, color = treatment)) +
  geom_point() +
  facet_wrap(~plot)


## species by species
AMBL = SBK %>%
  filter(species == "Amaranthus_blitoides") %>%
  filter(max.cover > 0)

ggplot(AMBL, aes(x=year, y=max.cover)) +
  geom_point() +
  facet_wrap(~plot)

ARPU = SBK %>%
  filter(species == "Aristida_purpurea") %>%
  filter(max.cover > 0)

BOBA = SBK %>%
  filter(species == "Bouteloua_barbata") %>%
  filter(max.cover > 0 )

ggplot(BOBA, aes(x=year, y=max.cover)) +
  geom_point() +
  facet_wrap(~plot)

# SBL ####
SBL = edge_all %>%
  filter(site == "SBL") %>%
  filter(species %in% c(SKsp))

sbl_check2 = sbl_check %>%
  filter(resp.ratio.site_D4 %in% c(1, -1, NaN) & resp.ratio.site_PDfull
         %in% c(1, -1, NaN))

## KNZ ####
KNZ = edge_all %>%
  filter(site == "KNZ") %>%
  filter(species %in% c(SKsp))

knz_check2 = knz_check %>%
  filter(resp.ratio.site_D4 %in% c(1, -1, NaN) & resp.ratio.site_PDfull
         %in% c(1, -1, NaN))


checks = KNZ %>%
  filter(species %in% c(unique(sbk_check2$species))) %>%
  filter(max.cover > 0)

one_obs_only = checks %>%
  group_by(species) %>%
  summarise(count = n())


