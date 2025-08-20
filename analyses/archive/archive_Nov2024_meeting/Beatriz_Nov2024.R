# Figure 2 by Functional Group and Life History
# November 13, 2024
# BAA

# source("analyses/calculate_response_ratio.R")
# source("analyses/color_palettes.R")
# source("analyses/response_ratio_analyses.R")

source("analyses/calc_response_ratio.R") 

#Read in functional group info
FG <- read.csv(here::here("data","edge_species_info_CP_BA.csv"))

#Join functional group data to species response ratio data
edge_RR2 <- edge_RR %>%
  left_join(FG, by = "species") %>%
  mutate(FunctionalGroup = ifelse(species %in% c("Astragalus_sp", "Eriogonum_sp", "Euphorbia_sp", "Oenothera_sp", "Asclepias_syriaca", "Cirsium_sp", "Astragalus_Oxytropis_sp"), "forb", 
                                  ifelse(species %in% c("Sporobolus_sp"), "grass", FunctionalGroup))) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))


nacheck = edge_RR2 %>%
  filter(is.na(FunctionalGroup))

sum_edge_RR2 = edge_RR2 %>%
  # group_by(site, species, FunctionalGroup, Duration, Photo) %>%
  # summarise(meanDRR = mean(resp.ratio.site_D, na.rm = T), 
  #           meanPDRR = mean(resp.ratio.site_PD, na.rm = T),
  #           seDRR = calcSE(resp.ratio.site_D),
  #           sePDRR = calcSE(resp.ratio.site_PD),
  #           persistence = median(persistence.site), 
  #           rank = median(percrank)) %>%
  
  mutate(spatial = ifelse(percrank > 0.75, "Abundant", "Scarce"),
         temporal = ifelse(persistence.site > 0.5, "Core", "Transient"),
         rarity_cat = paste0(temporal, ", ", spatial),
         MAP_level = ifelse(site %in% c("KNZ", "HYS"), "High", 
                            ifelse(site %in% c("CHY", "SGS"), "Intermediate", "Low"))) 

# CW 1/22/25 ####
## create quick new figs for shelved analyses section
## FG ####
ggplot(edge_RR2, aes(x=spatial_rarity, y=resp.ratio.site_D4, color = FunctionalGroup)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity") 
ggsave("figures/Jan2025/misc_not_included/FG_RR_v_rarity_drought_overall.png", width = 8, height = 3)

ggplot(edge_RR2, aes(x=temporal_rarity, y=resp.ratio.site_D4, color = FunctionalGroup)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Drought Response Ratio") +
  xlab("Temporal Rarity")
ggsave("figures/Jan2025/misc_not_included/FG_RR_v_Trarity_drought_overall.png", width = 8, height = 3)

ggplot(edge_RR2, aes(x=spatial_rarity, y=resp.ratio.site_D4, color = FunctionalGroup)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(site~FunctionalGroup) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity")
ggsave("figures/Jan2025/misc_not_included/FG_RR_v_rarity_drought.png", width = 8, height = 10)

ggplot(edge_RR2, aes(x=temporal_rarity, y=resp.ratio.site_D4, color = FunctionalGroup)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(site~FunctionalGroup) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Drought Response Ratio") +
  xlab("Temporal Rarity")
ggsave("figures/Jan2025/misc_not_included/FG_RR_v_Trarity_drought.png", width = 8, height = 10)

## Duration ####
edge_RR2 %>%
  filter(!Duration %in% c("unk", "annual/perennial"), !is.na(Duration)) %>%
ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity") 
ggsave("figures/Jan2025/misc_not_included/duration_RR_v_Srarity_drought.png", width = 6, height = 3)

edge_RR2 %>%
  filter(!Duration %in% c("unk", "annual/perennial"), !is.na(Duration)) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(site~Duration) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity") +
  theme_bw()
ggsave("figures/Jan2025/misc_not_included/duration_RR_v_Srarity_drought_site.png", width = 8, height = 10)

edge_RR2 %>%
  filter(!Duration %in% c("unk", "annual/perennial"), !is.na(Duration)) %>%
  ggplot(aes(x=temporal_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Temporal Rarity") 
ggsave("figures/Jan2025/misc_not_included/duration_RR_v_Trarity_drought.png", width = 6, height = 3)


edge_RR2 %>%
  filter(!Duration %in% c("unk", "annual/perennial"), !is.na(Duration)) %>%
  ggplot(aes(x=temporal_rarity, y=resp.ratio.site_D4, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(site~Duration) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Temporal Rarity") +
  theme_bw()
ggsave("figures/Jan2025/misc_not_included/duration_RR_v_Trarity_drought_site.png", width = 8, height = 10)

### post-drought ####
edge_RR2 %>%
  filter(!Duration %in% c("unk", "annual/perennial"), !is.na(Duration)) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_PDfull, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Post-Drought Response Ratio") +
  xlab("Spatial Rarity") 
ggsave("figures/Jan2025/misc_not_included/duration_RR_v_Srarity_postdrought.png", width = 6, height = 3)

edge_RR2 %>%
  filter(!Duration %in% c("unk", "annual/perennial"), !is.na(Duration)) %>%
  ggplot(aes(x=temporal_rarity, y=resp.ratio.site_PDfull, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Post-Drought Response Ratio") +
  xlab("Temporal Rarity") 
ggsave("figures/Jan2025/misc_not_included/duration_RR_v_Trarity_postdrought.png", width = 6, height = 3)










edge_RR2 %>%
  filter(!Duration %in% c("unk", "annual/perennial"), !is.na(Duration)) %>%
  ggplot(aes(x=spatial_rarity, y=resp.ratio.site_PDfull, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Post-Drought Response Ratio") +
  xlab("Spatial Rarity") 

edge_RR2 %>%
  filter(!Duration %in% c("unk", "annual/perennial"), !is.na(Duration)) %>%
  ggplot(aes(x=temporal_rarity, y=resp.ratio.site_PDfull, color = Duration)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration) +
  geom_hline(yintercept = 0, linetype = "dashed")  +
  ylab("Drought Response Ratio") +
  xlab("Spatial Rarity") 













ggplot(edge_RR2, aes(x=temporal_rarity, y=resp.ratio.site_D4, color = FunctionalGroup)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0, linetype = "dashed")

ggplot(edge_RR2, aes(x=spatial_rarity, y=resp.ratio.site_PDfull, color = FunctionalGroup)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0, linetype = "dashed")


# Figure 2 with functional groups
ggplot(edge_RR2, aes(x = spatial_rarity, y=temporal_rarity))+
  #geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  #geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(aes(color=FunctionalGroup), size = 1.5) +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15))# +
  #scale_x_reverse() +
  #scale_y_reverse() -> object

#object

ggplot(edge_RR2, aes(x = spatial_rarity, y=temporal_rarity, color=Duration))+
  #geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  #geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(aes(color=Duration), size = 1.5) +
  facet_grid(~site) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15))# +
  geom_smooth(method = "lm")


#create density plots for each rarity metric by functional group

library(ggExtra)

sum_edge_RR2 %>% 
  group_by(site) %>% 
  ggplot(aes(x=percrank, fill=FunctionalGroup, colour = FunctionalGroup)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()-> object2

object2

sum_edge_RR2 %>% 
  group_by(site) %>% 
  ggplot(aes(x=persistence.site, fill=FunctionalGroup, colour = FunctionalGroup)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Temporal Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()-> object3

object3


###############################
# Life History Figures
###############################

sum_edge_RR2 %>%
  group_by(site) %>%
  ggplot(aes(x=persistence.site, fill=Duration, colour = Duration)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Temporal Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() -> figure4

figure4


sum_edge_RR2 %>%
  group_by(site) %>%
  ggplot(aes(x=percrank, fill=Duration, colour = Duration)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() -> figure5

figure5


ggplot(sum_edge_RR2, aes(x = percrank, y=persistence.site))+
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

######################################################

# C3 vs. C4

sum_edge_RR2 %>% 
  filter(FunctionalGroup == "grass") -> grass

# Figure 2 with functional groups
ggplot(grass, aes(x = percrank, y=persistence.site))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(aes(color=Photo), size = 1.5) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse() -> figure7

figure7


grass %>% 
  filter(Photo != "unk") %>% 
  group_by(site) %>% 
  ggplot(aes(x=percrank, fill=Photo, colour = Photo)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()-> figure8

figure8

grass%>% 
  filter(Photo != "unk") %>% 
  group_by(site) %>% 
  ggplot(aes(x=persistence.site, fill=Photo, colour = Photo)) +
  geom_density(alpha=0.05) +
  facet_wrap(~MAP_level, ncol = 3, nrow = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Temporal Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()-> figure9

figure9



######################################################
# By Site

# Reorder sites
sum_edge_RR2$site <- factor(sum_edge_RR2$site, levels = c(
  "KNZ", "HYS", "CHY", "SGS", "SBK", "SBL"))

#Figure 2 by functional group by site
ggplot(sum_edge_RR2, aes(x = percrank, y=persistence.site))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(aes(color=FunctionalGroup), size = 1.5) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse() -> figure10

figure10


sum_edge_RR2 %>% 
  group_by(site) %>% 
  ggplot(aes(x=percrank, fill=FunctionalGroup, colour = FunctionalGroup)) +
  geom_density(alpha=0.05) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()-> figure11

figure11


sum_edge_RR2 %>% 
  group_by(site) %>% 
  ggplot(aes(x=persistence.site, fill=FunctionalGroup, colour = FunctionalGroup)) +
  geom_density(alpha=0.05) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Temporal Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()-> figure12

figure12



sum_edge_RR2 %>%
  group_by(site) %>%
  ggplot(aes(x=persistence.site, fill=Duration, colour = Duration)) +
  geom_density(alpha=0.05) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Temporal Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() -> figure13

figure13


sum_edge_RR2 %>%
  group_by(site) %>%
  ggplot(aes(x=percrank, fill=Duration, colour = Duration)) +
  geom_density(alpha=0.05) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() -> figure14

figure14


ggplot(sum_edge_RR2, aes(x = percrank, y=persistence.site))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(aes(color=Duration), size = 1.5) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse() -> figure15

figure15


# C3 vs. C4

sum_edge_RR2 %>% 
  filter(FunctionalGroup == "grass") -> grass

# Figure 2 with functional groups
ggplot(grass, aes(x = percrank, y=persistence.site))+
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  #geom_vline(xintercept = 0.5, color = "lightgray") +
  geom_vline(xintercept = 0.75, color = "red", linetype = "dashed") +
  geom_point(aes(color=Photo), size = 1.5) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse() +
  scale_y_reverse() -> figure16

figure16


grass %>% 
  filter(Photo != "unk") %>% 
  group_by(site) %>% 
  ggplot(aes(x=percrank, fill=Photo, colour = Photo)) +
  geom_density(alpha=0.05) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()-> figure17

figure17

grass%>% 
  filter(Photo != "unk") %>% 
  group_by(site) %>% 
  ggplot(aes(x=persistence.site, fill=Photo, colour = Photo)) +
  geom_density(alpha=0.05) +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Temporal Rarity")+
  ylab("Density") +
  theme(legend.position = "right") +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()-> figure18

figure18

######################################################
######################################################
#Save figures
# 
# object
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_withFG.jpeg"), width = 12, height = 5)
# 
# object2
# ggsave(here::here("Nov 2024 Meeting Figures","FG_SpatialRarity_Density.jpeg"), width = 12, height = 5)
# 
# object3
# ggsave(here::here("Nov 2024 Meeting Figures","FG_TemporalRarity_Density.jpeg"), width = 12, height = 5)
# 
# figure4
# ggsave(here::here("Nov 2024 Meeting Figures","FG_Temporal Rarity_Life history.jpeg"), width = 12, height = 5)
# 
# figure5
# ggsave(here::here("Nov 2024 Meeting Figures","FG_ Spatial Rarity_Life history.jpeg"), width = 12, height = 5)
# 
# figure6
# ggsave(here::here("Nov 2024 Meeting Figures","Figure2_Lifehistory.jpeg"), width = 12, height = 5)
# 
# figure7
# ggsave(here::here("Nov 2024 Meeting Figures","Figure2_GrassType.jpeg"), width = 12, height = 5)
# 
# figure8
# ggsave(here::here("Nov 2024 Meeting Figures","GrassType_SpatialRarity.jpeg"), width = 12, height = 5)
# 
# figure9
# ggsave(here::here("Nov 2024 Meeting Figures","GrassType_TemporalRarity.jpeg"), width = 12, height = 5)
# 
# figure10
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_site_FG.jpeg"), width = 12, height = 5)
# 
# figure11
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_site_FG_spatial.jpeg"), width = 12, height = 5)
# 
# figure12
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_site_FG_temporal.jpeg"), width = 12, height = 5)
# 
# figure13
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_site_lifehistory_temporal.jpeg"), width = 12, height = 5)
# 
# figure14
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_site_lifehistory_spatial.jpeg"), width = 12, height = 5)
# 
# figure15
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_site_lifehistory.jpeg"), width = 12, height = 5)
# 
# figure16
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_site_grass_C3C4.jpeg"), width = 12, height = 5)
# 
# figure17
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_site_grass_C3C4_spatial.jpeg"), width = 12, height = 5)
# 
# figure18
# ggsave(here::here("Nov 2024 Meeting Figures","Fig2_site_grass_C3C4_temporal.jpeg"), width = 12, height = 5)
# 
# 
