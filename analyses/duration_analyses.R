# Header ####
## Script name: 
##
## Purpose of script: 
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu


# Set up ####
source("analyses/calc_response_ratio.R") 
library(wesanderson)

tmppal2 = wes_palette("Darjeeling2", type = "discrete")
pal2 = c(tmppal2[3], "#94c0c1") #tmppal2[4])


#Read in functional group info
FG <- read.csv(here::here("data","edge_species_info_CP_BA.csv"))

#Join functional group data to species response ratio data
edge_RR2 <- edge_RR %>%
  left_join(FG, by = "species") %>%
  mutate(FunctionalGroup = ifelse(species %in% c("Astragalus_sp", "Eriogonum_sp", "Euphorbia_sp", "Oenothera_sp", "Asclepias_syriaca", "Cirsium_sp", "Astragalus_Oxytropis_sp"), "forb", 
                                  ifelse(species %in% c("Sporobolus_sp"), "grass", FunctionalGroup))) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"),
         Duration = ifelse(site == "KNZ" & species == "Asclepias_syriaca", "perennial", Duration),
         Duration = ifelse(species %in% c("Astragalus_drummondii", "Astragalus_laxmanii", "Astragalus_Oxytropis_sp", "Astragalus_shortianus", "Astragalus_sp", "Astragulus_crassicarpus"), "perennial", Duration),
         Duration = ifelse(species %in% c("Euphorbia_exstipulata", "Euphorbia_sp", "Euphorbia_sp."), "annual", Duration), 
         Duration = ifelse(species %in% c("Sporobolus_asper", "Sporobolus_cryptandrus", "Sporobolus_heterolepis", "Sporobolus_sp", "Sporobolus_sp."), "perennial", Duration), 
         Duration = ifelse(is.na(Duration) | Duration == "unk", "unknown", Duration))

# Check data ####
duration = edge_RR2 %>%
  filter(is.na(Duration) | Duration == "unk" | Duration == "annual/perennial")

astrag = FG %>%
  filter(genus == "astragalus")
## perennial

astrag_dat = edge_RR %>%
  filter(species %in% c("Astragalus_drummondii", "Astragalus_laxmanii", "Astragalus_Oxytropis_sp", "Astragalus_shortianus", "Astragalus_sp", "Astragulus_crassicarpus"))

sporob = FG %>%
  filter(genus == "sporobolus")
## perennial

sporobdat = edge_RR %>%
  filter(species %in% c("Sporobolus_asper", "Sporobolus_cryptandrus", "Sporobolus_heterolepis", "Sporobolus_sp", "Sporobolus_sp."))

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
edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = pal2)
#ggsave("figures/Jan2025/misc_not_included/duration_RR_v_Srarity_drought.png", width = 6, height = 3)

edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_PDfull, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = pal2)

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
ggsave("figures/Jan2025/duration_RR_v_Srarity_drought_site.png", width = 6, height = 8)


edge_RR2 %>%
  #filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4)) + #, color = Duration
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site, ncol = 1, nrow = 6) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity") +
  theme_bw()  +
  scale_color_manual(values = pal2)
ggsave("figures/Jan2025/RR_v_Srarity_drought_site.png", width = 3, height = 8)


edge_RR2 %>%
  filter(!Duration %in% c("unknown", "annual/perennial")) %>%
  ggplot(aes(x=temporal_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Temporal Rarity") 

edge_RR2 %>%
  group_by(site) %>%
  ggplot(aes(x=temporal_rarity, fill=Duration, colour = Duration)) +
  geom_density(alpha=0.05) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Temporal Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) #+
  #scale_x_reverse()






