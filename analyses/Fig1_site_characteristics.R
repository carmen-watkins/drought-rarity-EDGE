## Header ## 
## Script Name: Fig 1: Site Characteristics
##
##' Purpose of Script: Plot site characteristics: mean annual precipitation, 
##' mean annual temperature, average Berger-Parker dominance, and 
##' site proportions of annual / perennial and c3/c4 species. Figure 1 in 
##' Watkins et al. 2026.
##
## Author: Carmen Watkins
##

# Set up ####
## load packages
library(cowplot)

## read in data 
edge_RR = read.csv("data/edge_response_ratio_and_rarity.csv")
site_pred_scaled = read.csv("data/edge_site_predictors_all.csv")
FG = read.csv(here::here("data","edge_species_info_CP_BA.csv")) 

## set up graphics
global_size = 11
theme_set(theme_classic(base_size = global_size))

## define color palette
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


# Figure 1 ####
## plot precip by site
ppt = site_pred_scaled %>%
  mutate(site = fct_relevel(site, "SBK", "SBL", "SGS", "CHY", "HYS", "KNZ")) %>%
  ggplot(aes(x=site, y=MAP.mm, fill = site)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rev(pal)) +
  ylab("MAP (mm)") +
  xlab(" ") +
  theme(axis.title=element_text(size=11)) +
  labs(fill = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

## plot temp by site
temp = site_pred_scaled %>%
  mutate(site = fct_relevel(site, "SBK", "SBL", "SGS", "CHY", "HYS", "KNZ")) %>%
  ggplot(aes(x=site, y=MAT.C, fill = site)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rev(pal)) +
  ylab("MAT (C)") +
  xlab(" ") +
  theme(axis.title=element_text(size=11)) +
  labs(fill = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

## plot dominance by site
dom = site_pred_scaled %>%
  mutate(site = fct_relevel(site, "SBK", "SBL", "SGS", "CHY", "HYS", "KNZ")) %>%
  ggplot(aes(x=site, y=BP.dom.site, fill = site)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = BP.dom.site - se.dom.site, 
                    ymax = BP.dom.site + se.dom.site), width = 0.2) +
  scale_fill_manual(values = rev(pal))  +
  theme(axis.title=element_text(size=11)) +
  ylab("Mean Plot Dominance") +
  xlab(" ") +
  labs(fill = "Site") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

## plot the proportion of annual / perennial species by site
sctc = edge_RR2 %>%
  mutate(site = fct_relevel(site, "SBK", "SBL", "SGS", "CHY", "HYS", "KNZ")) %>%
  filter(spatial_rarity < 0.25 & temporal_rarity < 0.5) %>%
  group_by(site, Duration) %>%
  summarise(num_dur = n())%>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num_dur),
         prop_dur = num_dur / tot,
         Duration = ifelse(Duration == "annual", "A",
                           ifelse(Duration == "perennial", "P",
                                  ifelse(Duration == "unknown", "U", "A/P")))) %>%
  ggplot(aes(x=site, y=prop_dur, fill = Duration)) + 
  geom_bar(stat = 'identity', color = "black") +
  xlab("Site") +
  ylab("Proportion") +
  theme(axis.title=element_text(size=11)) +
  labs(fill = "Life History") +
  theme(plot.title = element_text(size = 11)) +
  scale_fill_manual(values = c("#020202", "white", "#DBDBDB", "#5F615E")) +
  ggtitle("Common, Persistent Species") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

## plot the proportion of c3/c4 species by site
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
  labs(fill = "PS Type") +
  theme(plot.title = element_text(size = 11)) +
  theme(axis.title=element_text(size=11)) +
  scale_fill_manual(values = c("#a7a7a7","#f2f2f2")) +
  ggtitle("Common, Persistent Grasses") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

## arrange ####
p1 = ggarrange(ppt, temp, dom, ncol = 3, common.legend = TRUE, legend = "right", 
               labels = c("a", "b", "c"), vjust = 1.1, hjust = 0.1,
               font.label=list(color="black",size=12))
p2 = ggarrange(sctc, cpg, labels = c("d", "e"),
               font.label=list(color="black",size=12))
plot = plot_grid(p1, p2, ncol = 1)

annotate_figure(plot, top = text_grob("Figure 1", 
                                      color = "black", face = "bold", size = 14))


## save official version
#ggsave("figures/final_figs/Fig1_site_char.tiff", width = 16, height = 12, 
## units = "cm", bg="white", dpi = 600)
