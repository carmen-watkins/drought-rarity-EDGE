# Header ####
## Script name: Annual Perennial Figures
##
## Purpose of script: ## Calc proportion of annual common vs. annual species at
## each site

##
## Author: Carmen Watkins
##

# Set up ####
source("analyses/calc_response_ratio.R") 
library(wesanderson)

tmppal2 = wes_palette("Darjeeling2", type = "discrete")
tmppal3 = wes_palette("Chevalier1")
#pal2 = c(tmppal2[3], "#94c0c1", tmppal3[1]) #tmppal2[4])

pal2 = c("#541A38", "#94c0c1")

#Read in functional group info
FG = read.csv(here::here("data","edge_species_info_CP_BA.csv"))

# Prep Data ####
#Join functional group data to species response ratio data
edge_RR2 <- edge_RR %>%
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

## check unknowns
photo_unks = edge_RR2 %>%
  filter(FunctionalGroup == "grass", is.na(Photo) | Photo == "unk")

## NAs are Sporobolus sp from SEV sites; from SEV species list, all 8 
##Sporobolus sp are C4; so safe to put htese as C4


## Calc percentage annual vs. perennial at a site

## Calc proportion of annual common vs. annual species at each site?
site_duration_props = edge_RR2 %>%
  group_by(site, Duration) %>%
  summarise(num_dur = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_dur),
         prop_dur = num_dur / tot)
  
ggplot(site_duration_props, aes(x= Duration, y= prop_dur, color = site)) +
  geom_point()
#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#CC3A8E,#A5AA99

# Check data ####
duration = edge_RR2 %>%
  filter(is.na(Duration) | Duration == "unk" | Duration == "annual/perennial")

astrag = FG %>%
  filter(genus == "astragalus")
## perennial

astrag_dat = edge_RR %>%
  filter(species %in% c("Astragalus_drummondii", "Astragalus_laxmanii", 
                        "Astragalus_Oxytropis_sp", "Astragalus_shortianus", 
                        "Astragalus_sp", "Astragulus_crassicarpus"))

sporob = FG %>%
  filter(genus == "sporobolus")
## perennial

sporobdat = edge_RR %>%
  filter(species %in% c("Sporobolus_asper", "Sporobolus_cryptandrus", 
                        "Sporobolus_heterolepis", "Sporobolus_sp",
                        "Sporobolus_sp."))

sort(unique(edge_RR$species))

euphorb = FG %>%
  filter(genus == "euphorbia")
## annual

oenoth = FG %>%
  filter(genus == "oenothera")
## mix of perennial & annual ; might be able to assign at some sites? depends which oenothera species are present

eriog = FG %>%
  filter(genus == "eriogonum") 
## mix of perennial & annual

## KNZ, Asclepias syriaca is unknown, but I think this is a perennial; Cirsium also unknown

# Plot ####
## Fig S5 ####
lh_sd = edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought") +
  xlab(" ") +
  labs(color = "Life History") +
  theme(text = element_text(size = 13)) +
  scale_color_manual(values = pal2) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.17", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.24", size = 3, parse = TRUE)

lh_td = edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=temporal_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab(" ") +
  xlab(" ") +
  theme(text = element_text(size = 13)) +
  scale_color_manual(values = pal2) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.16", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.20", size = 3, parse = TRUE)

lh_spd = edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_PDfull, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Post-Drought") +
  xlab("Spatial Rarity") +
  theme(text = element_text(size = 13)) +
  scale_color_manual(values = pal2) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.13", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.16", size = 3, parse = TRUE)

lh_tpd = edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=temporal_rarity, y=resp.ratio.site_PDfull, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  #facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab(" ") +
  xlab("Temporal Rarity") +
  theme(text = element_text(size = 13)) +
  scale_color_manual(values = pal2) +
  coord_cartesian(ylim = c(-1,1.2)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.11", size = 3, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.12", size = 3, parse = TRUE)

ggarrange(lh_sd, lh_td, lh_spd, lh_tpd, nrow = 2, ncol = 2, common.legend = TRUE, 
          legend = "bottom", labels = c("(a)", "(b)", "(c)", "(d)"))

#ggsave("figures/review_figs/FigS5_annual_perenn_RR_v_rarity.tiff", width = 6, height = 5.6)

## Talk Figure ####
edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity") +
  theme(text = element_text(size = 16)) +
  scale_color_manual(values = pal2) +
  labs(color = "Life History")

#ggsave("figures/dissertation_talk/duration.png", width = 8, height = 4)

## Fig S4 ####
all = ggplot(site_duration_props, aes(x=site, y=prop_dur, fill = Duration)) +
  geom_bar(stat = 'identity') +
  xlab(NULL) +
  ylab("Proportion") +
  ggtitle("All Species") +
  scale_fill_manual(values = c("#541A38", "#D69C4E", "#94c0c1", "#798E87"))

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
  ggtitle("Common & Persistent")  +
  scale_fill_manual(values = c("#541A38", "#D69C4E", "#94c0c1", "#798E87"))

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
  ylab("Proportion") +
  ggtitle("Sparse & Intermittent") +
  scale_fill_manual(values = c("#541A38", "#94c0c1", "#798E87"))

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
  ggtitle("Common & Intermittent") +
  scale_fill_manual(values = c("#541A38", "#94c0c1"))

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
  ylab("Proportion") +
  ggtitle("Sparse & Persistent") +
  scale_fill_manual(values = c("#541A38", "#D69C4E", "#94c0c1", "#798E87"))

ggarrange(all, sctc, srtr, srtc, sctr, common.legend = T, labels = c("(a)", "(b)", "(c)", "(d)", "(e)"), 
          legend = "bottom")

#ggsave("figures/review_figs/FigS4_sp_life_history_proportions.tiff", width = 8, height = 5)

## Exploratory C3/C4 Fig ####
all2 = edge_RR2 %>%
  filter(FunctionalGroup == "grass") %>%
  group_by(site, Photo) %>%
  summarise(num_photo = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_photo),
         prop_photo = num_photo / tot) %>%
  
  ggplot(aes(x=site, y=prop_photo, fill = Photo)) +
  geom_bar(stat = 'identity') +
  ylab("Proportion") +
  xlab("Site") +
  labs(fill = "Photosynthesis Pathway") +
  theme(text = element_text(size = 13)) +
  ggtitle("All Grasses") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], 
                               wes_palette("BottleRocket2")[1],
                               wes_palette("Darjeeling1")[3]))

cpg = edge_RR2 %>%
  filter(spatial_rarity < 0.25 & temporal_rarity < 0.5) %>%
  filter(FunctionalGroup == "grass") %>%
  group_by(site, Photo) %>%
  summarise(num_photo = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_photo),
         prop_photo = num_photo / tot) %>%
  ggplot(aes(x=site, y=prop_photo, fill = Photo)) +
  geom_bar(stat = 'identity') +
  ylab(" ") +
  xlab("Site") +
  theme(text = element_text(size = 13)) +
  ggtitle("Common, Persistent Grasses") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], 
                               wes_palette("BottleRocket2")[1], 
                               wes_palette("Darjeeling1")[3]))

ggarrange(all2, cpg, common.legend = TRUE, legend = "bottom", labels = "AUTO")

#ggsave("figures/Mar2025/FigS9_sp_duration_proportions.tiff", width = 7, height = 3.5)

edge_RR2 %>%
  filter(spatial_rarity >= 0.25 & temporal_rarity < 0.5) %>%
  filter(FunctionalGroup == "grass") %>%
  group_by(site, Photo) %>%
  summarise(num_photo = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_photo),
         prop_photo = num_photo / tot) %>%
  ggplot(aes(x=site, y=prop_photo, fill = Photo)) +
  geom_bar(stat = 'identity') +
  ylab(" ") +
  xlab("Site") +
  theme(text = element_text(size = 13)) +
  ggtitle("Sparse, Persistent Grasses") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], 
                               wes_palette("BottleRocket2")[1], 
                               wes_palette("Darjeeling1")[3]))


edge_RR2 %>%
  filter(spatial_rarity >= 0.25 & temporal_rarity >= 0.5) %>%
  filter(FunctionalGroup == "grass") %>%
  group_by(site, Photo) %>%
  summarise(num_photo = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_photo),
         prop_photo = num_photo / tot) %>%
  ggplot(aes(x=site, y=prop_photo, fill = Photo)) +
  geom_bar(stat = 'identity') +
  ylab(" ") +
  xlab("Site") +
  theme(text = element_text(size = 13)) +
  ggtitle("Sparse, Intermittent Grasses") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], 
                               wes_palette("BottleRocket2")[1], 
                               wes_palette("Darjeeling1")[3]))


edge_RR2 %>%
  filter(spatial_rarity < 0.25 & temporal_rarity >= 0.5) %>%
  filter(FunctionalGroup == "grass") %>%
  group_by(site, Photo) %>%
  summarise(num_photo = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_photo),
         prop_photo = num_photo / tot) %>%
  ggplot(aes(x=site, y=prop_photo, fill = Photo)) +
  geom_bar(stat = 'identity') +
  ylab(" ") +
  xlab("Site") +
  theme(text = element_text(size = 13)) +
  ggtitle("Common, Intermittent Grasses") +
  scale_fill_manual(values = c(wes_palette("BottleRocket2")[1], 
                               wes_palette("Darjeeling1")[3]))

## Site level ann/perenn ####
edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(site~Duration) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity") +
  theme_bw()  +
  scale_color_manual(values = pal2)
#ggsave("figures/Jan2025/duration_RR_v_Srarity_drought_site.png", width = 6, height = 8)

 