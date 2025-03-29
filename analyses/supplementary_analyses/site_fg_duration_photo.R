
# Set Up ####
library(wesanderson)

## read in data
source("analyses/calc_response_ratio.R") 
#source("analyses/color_palettes.R")

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



tmppal2 = wes_palette("Darjeeling2", type = "discrete")
tmppal3 = wes_palette("GrandBudapest2")
pal4 = wes_palette("Moonrise2")

pal2 = c(tmppal2[3], "#94c0c1", tmppal3[1], pal4[1]) #tmppal2[4])

"#D69C4E" "#94c0c1" "#E6A0C4" "#798E87"


# SBK ####
sbk = edge_RR2 %>%
  filter(site == "SBK")

## annual vs. perennial
sbkd = ggplot(sbk, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("") +
  scale_color_manual(values = pal2) 

sbkpd = ggplot(sbk, aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post-Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = pal2)

tsbk = ggarrange(sbkd, sbkpd, ncol = 1, common.legend = T, legend = "bottom", labels = "AUTO")

annotate_figure(tsbk, 
                fig.lab = "SBK", fig.lab.pos = "bottom.left")

ggsave("figures/Mar2025/site_fg_dur_photo/sbk_fgxdur.tiff", width = 8, height = 6)

## Photopath ####
pp_sbkd = sbk %>%
  filter(FunctionalGroup == "grass") %>%
  mutate(Photo = ifelse(species == "Sporobolus_sp", "c4", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("BottleRocket2")[1])) +
  ggtitle("SBK (Grasses)")

pp_sbkpd = sbk %>%
  filter(FunctionalGroup == "grass") %>%
  mutate(Photo = ifelse(species == "Sporobolus_sp", "c4", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c( wes_palette("BottleRocket2")[1])) +
  ggtitle(" ")

sbk_ppxdur = ggarrange(pp_sbkd, pp_sbkpd, legend = "none")

# SBL ####
sbl = edge_RR2 %>%
  filter(site == "SBL")

## annual vs. perennial
sbld = ggplot(sbl, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("") +
  scale_color_manual(values = pal2)

sblpd = ggplot(sbl, aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post-Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = pal2)

tsbl = ggarrange(sbld, sblpd, ncol = 1, common.legend = T, legend = "bottom", labels = "AUTO")

annotate_figure(tsbl, 
                fig.lab = "SBL", fig.lab.pos = "bottom.left")

ggsave("figures/Mar2025/site_fg_dur_photo/sbl_fgxdur.tiff", width = 8, height = 6)

## Photopath ####
pp_sbld = sbl %>%
  filter(FunctionalGroup == "grass") %>%
  mutate(Photo = ifelse(species == "Sporobolus_sp", "c4", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("BottleRocket2")[1])) +
  ggtitle("SBL (Grasses)")

pp_sblpd = sbl %>%
  filter(FunctionalGroup == "grass") %>%
  mutate(Photo = ifelse(species == "Sporobolus_sp", "c4", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("BottleRocket2")[1])) +
  ggtitle(" ")

sbl_ppxdur = ggarrange(pp_sbld, pp_sblpd, legend = "none")


# SGS ####
sgs = edge_RR2 %>%
  filter(site == "SGS")

## annual vs. perennial
sgsd = ggplot(sgs, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("") +
  scale_color_manual(values = c("#D69C4E", "#E6A0C4", "#94c0c1", "#798E87"))

sgspd = ggplot(sgs, aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post-Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c("#D69C4E", "#E6A0C4", "#94c0c1", "#798E87"))

tsgs = ggarrange(sgsd, sgspd, ncol = 1, common.legend = T, legend = "bottom", labels = "AUTO")

annotate_figure(tsgs, 
                fig.lab = "SGS", fig.lab.pos = "bottom.left")

ggsave("figures/Mar2025/site_fg_dur_photo/sgs_fgxdur.tiff", width = 8, height = 6)


## Photopath ####
pp_sgsd = sgs %>%
  filter(FunctionalGroup == "grass") %>%
  #mutate(Photo = ifelse(species == "Sporobolus_sp", "c4", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("BottleRocket2")[1])) +
  ggtitle("SGS (Grasses)")

pp_sgspd = sgs %>%
  filter(FunctionalGroup == "grass") %>%
  mutate(Photo = ifelse(species == "Sporobolus_sp", "c4", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("BottleRocket2")[1])) +
  ggtitle(" ")

sgs_ppxdur = ggarrange(pp_sgsd, pp_sgspd, legend = "none")


# CHY ####
chy = edge_RR2 %>%
  filter(site == "CHY")

## annual vs. perennial
chyd = ggplot(chy, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("") +
  scale_color_manual(values = c("#D69C4E", "#94c0c1", "#798E87"))

chypd = ggplot(chy, aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post-Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c("#D69C4E", "#94c0c1", "#798E87"))

tchy = ggarrange(chyd, chypd, ncol = 1, common.legend = T, legend = "bottom", labels = "AUTO")

annotate_figure(tchy, 
                fig.lab = "CHY", fig.lab.pos = "bottom.left")

ggsave("figures/Mar2025/site_fg_dur_photo/chy_fgxdur.tiff", width = 8, height = 6)

## Photopath ####
pp_chyd = chy %>%
  filter(FunctionalGroup == "grass") %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
 # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("BottleRocket2")[1])) +
  ggtitle("CHY (Grasses)")

pp_chypd = chy %>%
  filter(FunctionalGroup == "grass") %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  #facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post-Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("BottleRocket2")[1])) +
  ggtitle(" ")
 
chy_ppxdur = ggarrange(pp_chyd, pp_chypd, legend = "none")

#ggsave("figures/Mar2025/site_fg_dur_photo/chy_ppxdur.tiff", width = 6.5, height = 3)

# HYS ####
hys = edge_RR2 %>%
  filter(site == "HYS")

## annual vs. perennial
hysd = hys %>%
  
  filter(!is.na(FunctionalGroup)) %>%
  
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("") +
  scale_color_manual(values = c("#D69C4E", "#94c0c1", "#798E87"))


hyspd = hys %>%
  
  filter(!is.na(FunctionalGroup)) %>%
  
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post-Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c("#D69C4E", "#94c0c1", "#798E87"))

thys = ggarrange(hysd, hyspd, ncol = 1, common.legend = T, legend = "bottom", labels = "AUTO")

annotate_figure(thys, 
                fig.lab = "HYS", fig.lab.pos = "bottom.left")

ggsave("figures/Mar2025/site_fg_dur_photo/hys_fgxdur.tiff", width = 8, height = 6)


## Photopath ####
pp_hysd = hys %>%
  filter(FunctionalGroup == "grass") %>%
  #mutate(Photo = ifelse(species == "Sporobolus_sp", "c4", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("BottleRocket2")[1])) +
  ggtitle("HYS (Grasses)")

pp_hyspd = hys %>%
  filter(FunctionalGroup == "grass") %>%
  mutate(Photo = ifelse(species == "Sporobolus_sp", "c4", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("BottleRocket2")[1])) +
  ggtitle(" ")

hys_ppxdur = ggarrange(pp_hysd, pp_hyspd, legend = "none")

# KNZ ####
knz = edge_RR2 %>%
  filter(site == "KNZ")

## annual vs. perennial
knzd = ggplot(knz, aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("") +
  scale_color_manual(values = c("#D69C4E", "#E6A0C4", "#94c0c1", "#798E87"))

knzpd = ggplot(knz, aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Duration)) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post-Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c("#D69C4E", "#E6A0C4", "#94c0c1", "#798E87"))

tknz = ggarrange(knzd, knzpd, ncol = 1, common.legend = T, legend = "bottom", labels = "AUTO")

annotate_figure(tknz, 
                fig.lab = "KNZ", fig.lab.pos = "bottom.left")

ggsave("figures/Mar2025/site_fg_dur_photo/knz_fgxdur.tiff", width = 8, height = 6)


## Photopath ####
pp_knzd = knz %>%
  filter(FunctionalGroup == "grass") %>%
  mutate(Photo = ifelse(species == "Eleocharis_sp.", "c3", Photo),
         Photo = ifelse(species == "Juncus_interior", "c3", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_D4)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("BottleRocket2")[1])) +
  ggtitle("KNZ (Grasses)")

pp_knzpd = knz %>%
  filter(FunctionalGroup == "grass") %>%
  mutate(Photo = ifelse(species == "Eleocharis_sp.", "c3", Photo),
         Photo = ifelse(species == "Juncus_interior", "c3", Photo)) %>%
  ggplot(aes(x= spatial_rarity, y=resp.ratio.site_PDfull)) +
  # facet_wrap(~FunctionalGroup) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black") +
  geom_point(aes(color = Photo, shape = Duration), size = 2) +
  coord_cartesian(ylim = c(-1,1)) +
  ylab("Post Drought") +
  xlab("Spatial Rarity") +
  scale_color_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("BottleRocket2")[1])) +
  ggtitle(" ")

knz_ppxdur = ggarrange(pp_knzd, pp_knzpd, legend = "none")


ggarrange(pp_knzd, pp_knzpd, legend = "bottom", common.legend = TRUE)
ggsave("figures/Mar2025/site_fg_dur_photo/photo_path_legend.png", width = 6, height = 4)

ggarrange(knz_ppxdur, hys_ppxdur, chy_ppxdur, sgs_ppxdur, sbl_ppxdur, sbk_ppxdur, ncol= 1, common.legend = TRUE, labels = "AUTO")

ggsave("figures/Mar2025/site_fg_dur_photo/photo_path_grasses.png", width = 5, height = 10)
