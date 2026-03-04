# Header ####
## Script name: Funct Group Summaries

## Purpose of script: Explore proportion of each functional group that falls into
## each rarity category.
##
## Author: Beatriz Aguirre, Carmen Watkins
##

# Set Up ####
library(ggExtra)
library(cowplot)

source("analyses/calc_response_ratio.R") 

#Read in functional group info
FG = read.csv(here::here("data","edge_species_info_CP_BA.csv"))

#Join functional group data to species response ratio data
edge_RR2 = edge_RR %>%
  left_join(FG, by = "species") %>%
  mutate(FunctionalGroup = ifelse(species %in% c("Astragalus_sp", "Eriogonum_sp", 
                                                 "Euphorbia_sp", "Oenothera_sp", 
                                                 "Asclepias_syriaca", "Cirsium_sp", 
                                                 "Astragalus_Oxytropis_sp"), "forb", 
                                  ifelse(species %in% c("Sporobolus_sp"), 
                                         "grass", FunctionalGroup))) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"),
         Duration = ifelse(site == "KNZ" & species == "Asclepias_syriaca",
                           "perennial", Duration),
         Duration = ifelse(species %in% c("Astragalus_drummondii", 
                                          "Astragalus_laxmanii", 
                                          "Astragalus_Oxytropis_sp",
                                          "Astragalus_shortianus", 
                                          "Astragalus_sp", 
                                          "Astragulus_crassicarpus"), 
                           "perennial", Duration),
         Duration = ifelse(species %in% c("Euphorbia_exstipulata", 
                                          "Euphorbia_sp", "Euphorbia_sp."), 
                           "annual", Duration), 
         Duration = ifelse(species %in% c("Sporobolus_asper", 
                                          "Sporobolus_cryptandrus", 
                                          "Sporobolus_heterolepis", 
                                          "Sporobolus_sp", 
                                          "Sporobolus_sp."), 
                           "perennial", Duration), 
         Duration = ifelse(is.na(Duration) | Duration == "unk", 
                           "unknown", Duration)) %>%
  mutate(Photo = ifelse(site %in% c("SBK", "SBL") & species == "Sporobolus_sp",
                        "c4", Photo),
         Photo = ifelse(site == "KNZ" & species == "Eleocharis_sp.", 
                        "c3", Photo),
         Photo = ifelse(site == "KNZ" & species == "Juncus_interior", 
                        "c3", Photo))

edge_RR2_cats = edge_RR2 %>%
  mutate(spatial = ifelse(spatial_rarity < 0.25, "Common (S)", "Sparse (S)"),
         temporal = ifelse(temporal_rarity < 0.5, "Persistent (T)", "Intermittent (T)"),
         rarity_cat = paste0(spatial, ", ", temporal)) 

## arrange sites
edge_RR2_cats$site = factor(edge_RR2_cats$site, levels = c("KNZ", "HYS", "CHY", 
                                                         "SGS", "SBL", "SBK"))

edge_RR2_cats$rarity_cat = factor(edge_RR2_cats$rarity_cat, 
                                  levels = c("Common (S), Persistent (T)",  
                                             "Common (S), Intermittent (T)", 
                                             "Sparse (S), Persistent (T)", 
                                             "Sparse (S), Intermittent (T)"))


site_duration_props = edge_RR2 %>%
  group_by(site, Duration) %>%
  summarise(num_dur = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_dur),
         prop_dur = num_dur / tot)

nacheck = edge_RR2 %>%
  filter(is.na(FunctionalGroup))

site_fg_props = edge_RR2 %>%
  group_by(site, FunctionalGroup) %>%
  summarise(num_fg = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_fg),
         prop_fg = num_fg / tot)
## may want to look at within a site, how many forbs fall into each rarity category?

# Figure SXX ####
forbs = edge_RR2_cats %>%
  filter(FunctionalGroup == "forb") %>%
  group_by(site, rarity_cat) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = rarity_cat)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(" ") +
  #ylab("Proportion") +
  ggtitle("Forbs") +
  scale_fill_manual(values = c("#541A38", "#94c0c1", "#D69C4E", "#798E87"))

grasses = edge_RR2_cats %>%
  filter(FunctionalGroup == "grass") %>%
  group_by(site, rarity_cat) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = rarity_cat)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab("Proportion") +
  labs(fill = "Rarity Category") +
  ggtitle("Grasses") +
  scale_fill_manual(values = c("#541A38", "#94c0c1", "#D69C4E", "#798E87"))

shrubs = edge_RR2_cats %>%
  filter(FunctionalGroup == "shrub") %>%
  group_by(site, rarity_cat) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = rarity_cat)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(" ") +
 # ylab("Proportion") +
  ggtitle("Shrubs") +
  scale_fill_manual(values = c("#541A38", "#94c0c1", "#D69C4E", "#798E87"))

annuals = edge_RR2_cats %>%
  filter(Duration == "annual") %>%
  group_by(site, rarity_cat) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = rarity_cat)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab("Proportion") +
  labs(fill = "Rarity Categories") +
  ggtitle("Annuals") +
  scale_fill_manual(values = c("#541A38",  "#94c0c1", "#D69C4E", "#798E87"))

perennials = edge_RR2_cats %>%
  filter(Duration == "perennial") %>%
  group_by(site, rarity_cat) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = rarity_cat)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab("Proportion") +
  ggtitle("Perennials") +
  scale_fill_manual(values = c("#541A38", "#94c0c1", "#D69C4E", "#798E87"))


ggarrange(grasses, forbs, shrubs, annuals, perennials, common.legend = T, 
          legend = "right", ncol = 3, nrow = 2, 
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)"))

#ggsave("figures/review_figs/FigSXX_fg_lh_rarity_cat.tiff", width = 10, height = 5)

## Fig Alt version ####
all = ggplot(site_duration_props, aes(x=site, y=prop_dur, fill = Duration)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab("Proportion") +
  ggtitle("All Species") +
  labs(fill = "Life History") +
  scale_fill_manual(values = c("#541A38", "#D69C4E", "#94c0c1", "#FDDBD8")) +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15))

sctc = edge_RR2 %>%
  filter(spatial_rarity < 0.25 & temporal_rarity < 0.5) %>%
  group_by(site, Duration) %>%
  summarise(num_dur = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_dur),
         prop_dur = num_dur / tot) %>%
  ggplot(aes(x=site, y=prop_dur, fill = Duration)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab("Proportion") +
  ggtitle("Common; \nPersistent")  +
  scale_fill_manual(values = c("#541A38", "#D69C4E", "#94c0c1", "#FDDBD8")) +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1))

srtr = edge_RR2 %>%
  filter(spatial_rarity > 0.25 & temporal_rarity >= 0.5) %>%
  group_by(site, Duration) %>%
  summarise(num_dur = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_dur),
         prop_dur = num_dur / tot) %>%
  ggplot(aes(x=site, y=prop_dur, fill = Duration)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(" ") +
  ggtitle("Sparse; \nIntermittent") +
  scale_fill_manual(values = c("#541A38", "#94c0c1", "#FDDBD8")) +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1))

sctr = edge_RR2 %>%
  filter(spatial_rarity < 0.25 & temporal_rarity >= 0.5) %>%
  group_by(site, Duration) %>%
  summarise(num_dur = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_dur),
         prop_dur = num_dur / tot) %>% 
  ggplot(aes(x=site, y=prop_dur, fill = Duration)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab("Proportion") +
  ggtitle("Common; \nIntermittent") +
  scale_fill_manual(values = c("#541A38", "#94c0c1")) +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1))

srtc = edge_RR2 %>%
  filter(spatial_rarity >= 0.25 & temporal_rarity < 0.5) %>%
  group_by(site, Duration) %>%
  summarise(num_dur = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_dur),
         prop_dur = num_dur / tot) %>%
  ggplot(aes(x=site, y=prop_dur, fill = Duration)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(" ") +
  ggtitle("Sparse; \nPersistent") +
  scale_fill_manual(values = c("#541A38", "#D69C4E", "#94c0c1", "#FDDBD8")) +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1))

#ggsave("figures/review_figs/FigS4_sp_life_history_proportions.tiff", width = 8, height = 5)

## Fig S4 ####
all_fg = edge_RR2_cats %>%
  group_by(site, FunctionalGroup) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = FunctionalGroup)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(" ") +
  labs(fill = "Functional Group") +
  ggtitle("All Species") +
  scale_fill_manual(values = c("#494E65", "#C6CC62", "#798E87", "#A35B73")) +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15))

cspt = edge_RR2_cats %>%
  filter(rarity_cat == "Common (S), Persistent (T)") %>%
  group_by(site, FunctionalGroup) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = FunctionalGroup)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(" ") +
  ggtitle("Common; \nPersistent") +
  scale_fill_manual(values = c("#494E65", "#C6CC62", "#798E87", "#A35B73")) +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1))

sspt = edge_RR2_cats %>%
  filter(rarity_cat == "Sparse (S), Persistent (T)") %>%
  group_by(site, FunctionalGroup) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = FunctionalGroup)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(" ") +
  labs(fill = "Functional Group") +
  ggtitle("Sparse; \nPersistent") +
  scale_fill_manual(values = c("#494E65", "#C6CC62", "#798E87", "#A35B73"))  +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1))

csit = edge_RR2_cats %>%
  filter(rarity_cat == "Common (S), Intermittent (T)") %>%
  group_by(site, FunctionalGroup) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = FunctionalGroup)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(" ") +
  ggtitle("Common; \nIntermittent") +
  scale_fill_manual(values = c("#494E65", "#C6CC62", "#798E87", "#A35B73")) +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1))

ssit = edge_RR2_cats %>%
  filter(rarity_cat == "Sparse (S), Intermittent (T)") %>%
  group_by(site, FunctionalGroup) %>%
  summarise(numpcat = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(numpcat),
         prop_fg = numpcat / tot) %>%
  ggplot(aes(x=site, y=prop_fg, fill = FunctionalGroup)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab(" ") +
  labs(fill = "Rarity Categories") +
  ggtitle("Sparse; \nIntermittent") +
  scale_fill_manual(values = c("#494E65", "#C6CC62", "#798E87", "#A35B73")) +
  theme(text=element_text(size=15),
        plot.title = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust = 1))

p1 = ggarrange(all, all_fg, ncol = 2, nrow = 1, 
               labels = c("a", "f"))

p2 = ggarrange(sctc, srtc, sctr, srtr,
               labels = c("b", "c", "d", "e", "f"), legend = "none")

p3 = ggarrange(cspt, sspt, csit, ssit, common.legend = T, 
          legend = "none", ncol = 2, nrow = 2, 
          labels = c("g", "h", "i", "j"))

p4 = plot_grid(p2, p3)

plot_grid(p1, p4, ncol = 1, rel_heights = c(0.5, 1))

ggsave("figures/review_figs/FigS4_fg_lh_rarity_cat.tiff",
       width = 10.5, height = 7.5)


ggsave("figures/review_figs/supp/FigS4_fg_lh_rarity_cat.png",
       width = 10.5, height = 7.5)





# Explore 'iconic' sp ####
knzSI = edge_RR2_cats %>%
  filter(site == "KNZ") %>%
  filter(rarity_cat == "Sparse (S), Intermittent (T)")

unique(knzSI$species)
## Achillea millefolium
Erigeron_strigosus
Euphorbia_marginata
Lespedeza_violacea
Asclepias_syriaca
Bouteloua_dactyloides

knzSP = edge_RR2_cats %>%
  filter(site == "KNZ") %>%
  filter(rarity_cat == "Sparse (S), Persistent (T)")
Koeleria_macrantha
Baptisia_bracteata
Bouteloua_gracilis
Artemesia_ludoviciana
Solidago_canadensis
Dalea_purpurea

knzCI = edge_RR2_cats %>%
  filter(site == "KNZ") %>%
  filter(rarity_cat == "Common (S), Intermittent (T)")

knzCP = edge_RR2_cats %>%
  filter(site == "KNZ") %>%
  filter(rarity_cat == "Common (S), Persistent (T)")

## hys ####
hysSIf = edge_RR2_cats %>%
  filter(site == "HYS") %>%
  filter(rarity_cat == "Sparse (S), Intermittent (T)",
         FunctionalGroup == "forb")

## forb sp 
## Solidago_missouriensis
## Ipomopsis_laxiflora

## Helianthus_annuus
## Solidago_canadensis
## Draba_reptans

hysSIg = edge_RR2_cats %>%
  filter(site == "HYS") %>%
  filter(rarity_cat == "Sparse (S), Intermittent (T)",
         FunctionalGroup == "grass")

hysSIs = edge_RR2_cats %>%
  filter(site == "HYS") %>%
  filter(rarity_cat == "Sparse (S), Intermittent (T)",
         FunctionalGroup == "shrub")

## Erysimum_capitatum
## Chaetopappa_ericoides
## Castilleja_purpurea

hys = edge_RR2_cats %>%
  filter(site == "HYS") %>%
  select(site, species, spatial_rarity, temporal_rarity, FunctionalGroup, rarity_cat)

write.csv(hys, "species_lists/hys_sp_list.csv", row.names = F)

sgs = edge_RR2_cats %>%
  filter(site == "SGS") %>%
  select(site, species, spatial_rarity, temporal_rarity, FunctionalGroup, rarity_cat)

write.csv(sgs, "species_lists/sgs_sp_list.csv", row.names = F)

chy = edge_RR2_cats %>%
  filter(site == "CHY") %>%
  select(site, species, spatial_rarity, temporal_rarity, FunctionalGroup, rarity_cat)

write.csv(chy, "species_lists/chy_sp_list.csv", row.names = F)


knz = edge_RR2_cats %>%
  filter(site == "KNZ") %>%
  select(site, species, spatial_rarity, temporal_rarity, FunctionalGroup, rarity_cat)

write.csv(knz, "species_lists/knz_sp_list.csv", row.names = F)


sbl = edge_RR2_cats %>%
  filter(site == "SBL") %>%
  select(site, species, spatial_rarity, temporal_rarity, FunctionalGroup, rarity_cat)

write.csv(sbl, "species_lists/sbl_sp_list.csv", row.names = F)

sbk = edge_RR2_cats %>%
  filter(site == "SBK") %>%
  select(site, species, spatial_rarity, temporal_rarity, FunctionalGroup, rarity_cat)

write.csv(sbk, "species_lists/sbk_sp_list.csv", row.names = F)
