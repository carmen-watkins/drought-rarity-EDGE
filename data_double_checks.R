## Header 
##
## Data double checks after talk with Mendy
##

## SS: explore Mendy's proposed checks - are -1's and 1's present in both control + treatment plots in pre-treat data? 
## sites with little or large compositional change
## ANGE at KNZ increased during drought - where is this on the plots now??

## check Andropogon ####
## should have increased at KNZ
ANGE = edge_RR %>%
  filter(species == "Andropogon_gerardii")
## yep, ANGE increased at KNZ, but not at HYS where it is also found

## check species that dropped out at SGS ####
sgs_explore = edge_RR2 %>%
  filter(site == "SGS", resp.ratio.site_D4 < 0, spatial_rarity > 0.3)

sort(unique(sgs_explore$species))
sort(unique(sgs_explore$FunctionalGroup))

table(sgs_explore$FunctionalGroup)
table(sgs_explore$Duration)
table(sgs_explore$genus)
table(sgs_explore$resp.ratio.site_D4)

sgs_do = sgs_explore %>%
  filter(resp.ratio.site_D4 == -1)

ARPU = edge_RR2 %>%
  filter(species == "Aristida_purpurea")

ggplot(ARPU, aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point()

ASSH = edge_RR2 %>%
  filter(species == "Astragalus_shortianus")

edge_RR2 %>%
  group_by(species) %>%
  mutate(count = n()) %>%
  filter(count > 1) %>%
  filter(Duration == "perennial", !is.na(resp.ratio.site_D4), FunctionalGroup == "grass") %>%
  ggplot(aes(x = spatial_rarity, y = resp.ratio.site_D4)) +
  geom_point() +
  facet_wrap(~species) +
  geom_smooth(method = "lm")
