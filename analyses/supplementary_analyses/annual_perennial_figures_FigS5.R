# Header ####
## Script name: Annual Perennial Figures
##
## Purpose of script: ## Calc proportion of annual common vs. annual species at
## each site

##
## Author: Carmen Watkins
##

# Set up ####
## read in data
edge_RR = read.csv("data/edge_response_ratio_and_rarity.csv")
FG = read.csv(here::here("data","edge_species_info_CP_BA.csv"))

## set color palette
pal2 = c("#541A38", "#94c0c1")

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
##Sporobolus sp are C4; so safe to put these as C4

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
  geom_point(size = 1) +
  geom_smooth(method = "lm") +
  #facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought") +
  xlab(" ") +
  labs(color = "Life History") +
  theme(text = element_text(size = 13)) +
  scale_color_manual(values = pal2) +
  coord_cartesian(ylim = c(-1,1.6)) +
  annotate("text", x = 0.1, y=1.16, label = "R[m]^2: 0.17", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.4, y=1.16, label = "R[c]^2: 0.24", size = 3.5, parse = TRUE) +
  annotate("text", x = 0.25, y=1.38, label = "A: 0.27 [-0.13, 0.67]", size = 3.5, parse = FALSE) +
  annotate("text", x = 0.25, y=1.58, label = "P: 1.06 [0.83, 1.3]", size = 3.5, parse = FALSE) 

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
          legend = "bottom", labels = c("a", "b", "c", "d"))

#ggsave("figures/final_figs/supp_figs/FigS5_annual_perenn_RR_v_rarity.png", width = 18, height = 16.5, units = "cm")

#ggsave("figures/review_figs/supp/FigS5_annual_perenn_RR_v_rarity.png", width = 6, height = 5.6)
