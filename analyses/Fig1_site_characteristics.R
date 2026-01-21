
## Header ## 
## Script Name: Fig 1: Site Characteristics

## Purpose of Script: create a figure of site MAP, temp, average BP dominance, 
## and annual / perennial and c3/c4

## Author: Carmen Watkins

# Set up ####
## load packages
library(cowplot)

## read in data 
source("analyses/calc_response_ratio.R") ## response ratio data
source("data-prep/prep_model_predictors.R") ## site ppt, temp, dominance data
source("analyses/color_palettes.R") ## for color palettes

#Read in functional group info
FG = read.csv(here::here("data","edge_species_info_CP_BA.csv")) 

## set up graphics
theme_set(theme_classic())
#pal <- wes_palette("Royal3")
## new color palette
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

# Prep Data ####
#Join functional group data to species response ratio data
edge_RR2 = edge_RR %>%
  left_join(FG, by = "species") %>%
  
  ## manually edit FG & duration
  mutate(FunctionalGroup = ifelse(species %in% c("Astragalus_sp", "Eriogonum_sp", 
                                                 "Euphorbia_sp", "Oenothera_sp", 
                                                 "Asclepias_syriaca", "Cirsium_sp",
                                                 "Astragalus_Oxytropis_sp"), "forb", 
                                  ifelse(species %in% c("Sporobolus_sp"), "grass", 
                                         FunctionalGroup))) %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"),
         Duration = ifelse(site == "KNZ" & species == "Asclepias_syriaca", 
                           "perennial", Duration),
         Duration = ifelse(species %in% c("Astragalus_drummondii", 
                                          "Astragalus_laxmanii", 
                                          "Astragalus_Oxytropis_sp",
                                          "Astragalus_shortianus", "Astragalus_sp",
                                          "Astragulus_crassicarpus"),
                           "perennial", Duration),
         Duration = ifelse(species %in% c("Euphorbia_exstipulata", "Euphorbia_sp", 
                                          "Euphorbia_sp."), "annual", Duration), 
         Duration = ifelse(species %in% c("Sporobolus_asper", "Sporobolus_cryptandrus",
                                          "Sporobolus_heterolepis", "Sporobolus_sp", 
                                          "Sporobolus_sp."), 
                           "perennial", Duration), 
         Duration = ifelse(is.na(Duration) | Duration == "unk", "unknown", 
                           Duration)) %>%
  mutate(Photo = ifelse(site %in% c("SBK", "SBL") & species == "Sporobolus_sp",
                        "c4", Photo),
         Photo = ifelse(site == "KNZ" & species == "Eleocharis_sp.", "c3", Photo),
         Photo = ifelse(site == "KNZ" & species == "Juncus_interior", "c3", Photo))


# Figures ####
## Fig 1 ####
ppt = site_pred_scaled %>%
  mutate(site = fct_relevel(site, "SBK", "SBL", "SGS", "CHY", "HYS", "KNZ")) %>%
  ggplot(aes(x=site, y=MAP.mm, fill = site)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rev(pal)) +
  ylab("Mean Annual Precip (mm)") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  labs(fill = "Site")

temp = site_pred_scaled %>%
  mutate(site = fct_relevel(site, "SBK", "SBL", "SGS", "CHY", "HYS", "KNZ")) %>%
  ggplot(aes(x=site, y=MAT.C, fill = site)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rev(pal)) +
  ylab("Mean Annual Temp (C)") +
  xlab(" ") +
  theme(text = element_text(size = 15)) +
  labs(fill = "Site")

dom = site_pred_scaled %>%
  mutate(site = fct_relevel(site, "SBK", "SBL", "SGS", "CHY", "HYS", "KNZ")) %>%
  ggplot(aes(x=site, y=BP.dom.site, fill = site)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = BP.dom.site - se.dom.site, 
                    ymax = BP.dom.site + se.dom.site), width = 0.2) +
  scale_fill_manual(values = rev(pal))  +
  theme(text = element_text(size = 15)) +
  ylab("Mean Plot Dominance") +
  xlab(" ") +
  labs(fill = "Site")

sctc = edge_RR2 %>%
  mutate(site = fct_relevel(site, "SBK", "SBL", "SGS", "CHY", "HYS", "KNZ")) %>%
  filter(spatial_rarity < 0.25 & temporal_rarity < 0.5) %>%
  group_by(site, Duration) %>%
  summarise(num_dur = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_dur),
         prop_dur = num_dur / tot,
         Duration = ifelse(Duration == "annual", "Ann", 
                           ifelse(Duration == "annual/perennial", "Ann/Peren", 
                                  ifelse(Duration == "perennial", "Peren", 
                                         "Unk")))) %>%
  ggplot(aes(x=site, y=prop_dur, fill = Duration)) +
  geom_bar(stat = 'identity', color = "black") +
  xlab("Site") +
  ylab("Proportion") +
  theme(text = element_text(size = 15)) +  
  labs(fill = NULL) +
  scale_fill_manual(values = c("#020202", "#c0c0c0","#767676", "#494949")) +
  ggtitle("Common, Persistent Species")

cpg = edge_RR2 %>%
  mutate(site = fct_relevel(site, "SBK", "SBL", "SGS", "CHY", "HYS", "KNZ")) %>%
  filter(spatial_rarity < 0.25 & temporal_rarity < 0.5) %>%
  filter(FunctionalGroup == "grass") %>%
  group_by(site, Photo) %>%
  summarise(num_photo = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_photo),
         prop_photo = num_photo / tot,
         Photo = toupper(Photo)) %>%
  ggplot(aes(x=site, y=prop_photo, fill = Photo)) +
  geom_bar(stat = 'identity', color = "black") +
  ylab(" ") +
  xlab("Site") +
  labs(fill = NULL) +
  theme(text = element_text(size = 15)) +
  scale_fill_manual(values = c("#a7a7a7","#f2f2f2")) +
  ggtitle("Common, Persistent Grasses")

p1 = ggarrange(ppt, temp, dom, ncol = 3, common.legend = TRUE, legend = "right", 
               labels = c("(a)", "(b)", "(c)"))
p2 = plot_grid(sctc, cpg, labels = c("(d)", "(e)"), rel_widths = c(1.1, 1))
plot_grid(p1, p2, ncol = 1)

#ggsave("figures/review_figs/Fig1_site_char.tiff", width = 10, height = 6.75)
