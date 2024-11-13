#Read in functional group info
setwd("~/Documents/NCEAS_Transitions/subordinate-species/data")
FG <- read.csv("edge_species_info_CP_BA.csv")

#add functional group to species
sp.list.FG <- left_join(sp.list, FG, by = c("species"))

sp.list.FG %>%
  group_by(species) %>%
  select(species, FunctionalGroup, Duration) -> sp.list.FG

# Remove duplicate rows based on species
sp.list.FG <- sp.list.FG %>%
  distinct(species, .keep_all = TRUE)


edge_RR2 <- edge_RR %>%
  left_join(sp.list.FG, by = "species")


sum_edge_RR2 = edge_RR2 %>%
  group_by(site, species, FunctionalGroup) %>%
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
ggsave("~/Documents/NCEAS_Transitions/subordinate-species/Nov 2024 Meeting Figures/FG_SpatialRarity_Density.jpeg", width = 10, height = 5)


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

ggsave("~/Documents/NCEAS_Transitions/subordinate-species/Nov 2024 Meeting Figures/FG_TemporalRarity_Density.jpeg", width = 10, height = 5)



sum_edge_RR2 %>% 
  group_by(FunctionalGroup) %>% 
  filter(is.na(FunctionalGroup)) -> unknown.observations
