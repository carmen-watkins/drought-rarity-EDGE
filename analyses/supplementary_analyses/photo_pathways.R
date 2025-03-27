
photo = read.csv("data/Taxa_Info.csv") %>%
  mutate(species = paste0(Genus, "_", Species)) 
## fixed Schizacharium and Sorghastrum which were wrong; will be hard to know if there are other mistakes


## read in all of sev data
sev_edge <- read.csv("data/sev_download/sev298_NPP_edge_biomass.csv")

## read in species info data
FG <- read.csv(here::here("data","edge_species_info_CP_BA.csv"))


## select just species info
sev_sp_info = sev_edge %>%
  select(kartez, genus, sp.epithet, family, LifeHistory, PhotoPath, FunctionalGroup) %>%
  distinct() %>%
  mutate(species = paste0(genus, "_", sp.epithet))

## rename columns
names(sev_sp_info) = c("kartez", "genus", "sp.epithet", "family", "Duration", "Photo2", "FunctionalGroup", "species")

photo2 = left_join(FG, sev_sp_info, by = c("species"))


common.sp = unique(photo2$species)


sev2 = sev_sp_info %>%
  filter(!species %in% c(common.sp)) %>%
  mutate(Photo = NA, 
         CorrectSpelling = NA, 
         Notes = NA, 
         genus.x = genus,
         genus.y = NA,
         Duration.x = Duration,
         Duration.y = NA,
         FunctionalGroup.x = FunctionalGroup, 
         FunctionalGroup.y = NA) %>%
  select(-genus, -Duration, -FunctionalGroup)

all_sp = rbind(photo2, sev2) %>%
  mutate(photo_comb = ifelse(Photo %in% c("", "unk") | is.na(Photo), Photo2, Photo),
         photo_comb = toupper(photo_comb),
         FunctionalGroup = ifelse(FunctionalGroup.x == FunctionalGroup.y | is.na(FunctionalGroup.y), FunctionalGroup.x,
                                  ifelse( is.na(FunctionalGroup.x), 
                                          FunctionalGroup.y,
                                          
                                          "mismatch")),
         Duration = ifelse(Duration.x == Duration.y | is.na(Duration.y), Duration.x,
                           ifelse( is.na(Duration.x), 
                                   Duration.y,
                                   
                                   "mismatch")))

table(all_sp$photo_comb)


all_sp_mistakes = all_sp %>%
  select(species, FunctionalGroup, Duration, photo_comb) %>%
  distinct()


edge_RR2 <- edge_RR %>%
  left_join(all_sp_mistakes, by = "species") %>%
  mutate(FunctionalGroup = ifelse(species %in% c("Astragalus_sp", "Eriogonum_sp", "Euphorbia_sp", "Oenothera_sp", "Asclepias_syriaca", "Cirsium_sp", "Astragalus_Oxytropis_sp"), "forb", 
                                  ifelse(species %in% c("Sporobolus_sp"), "grass", FunctionalGroup))) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"),
         Duration = ifelse(site == "KNZ" & species == "Asclepias_syriaca", "perennial", Duration),
         Duration = ifelse(species %in% c("Astragalus_drummondii", "Astragalus_laxmanii", "Astragalus_Oxytropis_sp", "Astragalus_shortianus", "Astragalus_sp", "Astragulus_crassicarpus"), "perennial", Duration),
         Duration = ifelse(species %in% c("Euphorbia_exstipulata", "Euphorbia_sp", "Euphorbia_sp."), "annual", Duration), 
         Duration = ifelse(species %in% c("Sporobolus_asper", "Sporobolus_cryptandrus", "Sporobolus_heterolepis", "Sporobolus_sp", "Sporobolus_sp."), "perennial", Duration), 
         Duration = ifelse(is.na(Duration) | Duration == "unk", "unknown", Duration))



edge_RR2 %>%
  filter(FunctionalGroup == "grass") %>%
ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4, color = photo_comb)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~photo_comb) +
  geom_hline(yintercept = 0)

edge_RR2 %>%
  filter(FunctionalGroup == "forb") %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4, color = photo_comb)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~photo_comb) +
  geom_hline(yintercept = 0)


ggplot(edge_RR2, aes(x=site, fill = photo_comb)) +
  geom_bar()
  

edge_RR2 %>%
  filter(site %in% c("SBK", "SBL")) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4, color = photo_comb)) +
 
  geom_smooth(method = "lm", alpha = 0.25) +
  geom_point() +
  facet_wrap(~site) +
  geom_hline(yintercept = 0)


