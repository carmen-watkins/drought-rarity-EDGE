# Figure 2 by Functinal Group and Life History
# November 13, 2024
# BAA

source("analyses/calculate_response_ratio.R")
source("analyses/color_palettes.R")
source("analyses/response_ratio_analyses.R")

#Read in functional group info
FG <- read.csv(here::here("data","edge_species_info_CP_BA.csv"))

#Join functional group data to species response ratio data
edge_RR2 <- edge_RR %>%
  left_join(FG, by = "species")

sum_edge_RR2 = edge_RR2 %>%
  group_by(site, species, FunctionalGroup, Duration) %>%
  summarise(meanDRR = mean(resp.ratio.site_D, na.rm = T), 
            meanPDRR = mean(resp.ratio.site_PD, na.rm = T),
            seDRR = calcSE(resp.ratio.site_D),
            sePDRR = calcSE(resp.ratio.site_PD),
            persistence = median(persistence.site), 
            rank = median(percrank)) %>%
  
  mutate(spatial = ifelse(rank > 0.75, "Abundant", "Scarce"),
         temporal = ifelse(persistence > 0.5, "Core", "Transient"),
         rarity_cat = paste0(temporal, ", ", spatial),
         MAP_level = ifelse(site %in% c("KNZ", "HYS"), "High", 
                            ifelse(site %in% c("CHY", "SGS"), "Intermediate", "Low"))) 

# Figure 2 with functional groups
ggplot(sum_edge_RR2, aes(x = rank, y=persistence))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(aes(color=FunctionalGroup), size = 1.5) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse() -> object

object


#create density plots for each rarity metric by functional group

library(ggExtra)

sum_edge_RR2 %>% 
  group_by(site) %>% 
  ggplot(aes(x=rank, fill=FunctionalGroup, colour = FunctionalGroup)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) -> object2

object2
#ggsave("~/Documents/NCEAS_Transitions/subordinate-species/Nov 2024 Meeting Figures/FG_SpatialRarity_Density.jpeg", width = 12, height = 5)


sum_edge_RR2 %>% 
  group_by(site) %>% 
  ggplot(aes(x=persistence, fill=FunctionalGroup, colour = FunctionalGroup)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Temporal Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) -> object3

object3
#ggsave("~/Documents/NCEAS_Transitions/subordinate-species/Nov 2024 Meeting Figures/FG_TemporalRarity_Density.jpeg", width = 12, height = 5)


###############################
# Life History Figures
###############################

sum_edge_RR2 %>%
  group_by(site) %>%
  ggplot(aes(x=persistence, fill=Duration, colour = Duration)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Temporal Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) -> figure4

figure4


sum_edge_RR2 %>%
  group_by(site) %>%
  ggplot(aes(x=rank, fill=Duration, colour = Duration)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) -> figure5

figure5


ggplot(sum_edge_RR2, aes(x = rank, y=persistence))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(aes(color=Duration), size = 1.5) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse() -> figure6

figure6


######################################################
# Check if functional group NAs are gone:
sum_edge_RR2 %>% 
  group_by(FunctionalGroup) %>% 
  filter(is.na(FunctionalGroup)) -> unknown.observations
