# Header ####
## Script name: Categorization sensitivity analysis

##' Purpose of script: Explore whether moving the bounds of rarity categories
##' changes the findings from Fig 3
##
## Author: Carmen Watkins
##

# Set up ####
source("analyses/calc_response_ratio.R") 

library(ggpubr)

## set up graphics
theme_set(theme_classic())

# Define Rarity Categories ####
## as in figure 3
edge_RR_cats = edge_RR %>%
  mutate(spatial = ifelse(spatial_rarity < 0.25, "Sp Common", "Sp Rare"),
         temporal = ifelse(temporal_rarity < 0.5, "Tmp Common", "Tmp Rare"),
         rarity_cat = paste0(temporal, ", ", spatial)) %>%
  filter(!is.na(resp.ratio.site_D4), !is.na(resp.ratio.site_PDfull)) %>%
  group_by(rarity_cat) %>%
  summarise(corr_RR = cor(resp.ratio.site_D4, resp.ratio.site_PDfull))

## narrow definition of common/persistent species
edge_RR_cats_low = edge_RR %>%
  mutate(spatial = ifelse(spatial_rarity < 0.15, "Sp Common", "Sp Rare"),
         temporal = ifelse(temporal_rarity < 0.4, "Tmp Common", "Tmp Rare"),
         rarity_cat = paste0(temporal, ", ", spatial)) %>%
  filter(!is.na(resp.ratio.site_D4), !is.na(resp.ratio.site_PDfull)) %>%
  group_by(rarity_cat) %>%
  summarise(corr_RR = cor(resp.ratio.site_D4, resp.ratio.site_PDfull))

## broad definition of common/persistent species
edge_RR_cats_hi = edge_RR %>%
  mutate(spatial = ifelse(spatial_rarity < 0.35, "Sp Common", "Sp Rare"),
         temporal = ifelse(temporal_rarity < 0.6, "Tmp Common", "Tmp Rare"),
         rarity_cat = paste0(temporal, ", ", spatial)) %>%
  filter(!is.na(resp.ratio.site_D4), !is.na(resp.ratio.site_PDfull)) %>%
  group_by(rarity_cat) %>%
  summarise(corr_RR = cor(resp.ratio.site_D4, resp.ratio.site_PDfull))




## arrange sites
edge_RR_cats$site = factor(edge_RR_cats$site, levels = c("KNZ", "HYS", "CHY", 
                                                         "SGS", "SBL", "SBK"))

edge_RR_cats_low$site = factor(edge_RR_cats_low$site, 
                               levels = c("KNZ", "HYS", "CHY", "SGS", 
                                          "SBL", "SBK"))

edge_RR_cats_hi$site = factor(edge_RR_cats_hi$site, 
                              levels = c("KNZ", "HYS", "CHY", "SGS", 
                                         "SBL", "SBK"))

# Figure 3 ####
edge_RR_cats %>%
  mutate(rarity_2 = ifelse(rarity_cat == "Tmp Rare, Sp Common", 
                           "Common (S), Intermittent (T)",
                           ifelse(rarity_cat == "Tmp Rare, Sp Rare", 
                                  "Sparse (S), Intermittent (T)", 
                                  ifelse(rarity_cat == "Tmp Common, Sp Common", 
                                         "Common (S), Persistent (T)", 
                                         "Sparse (S), Persistent (T)"))), 
         
         rarity_2 = fct_relevel(rarity_2, "Common (S), Persistent (T)", 
                                "Sparse (S), Persistent (T)",  
                                "Common (S), Intermittent (T)", 
                                "Sparse (S), Intermittent (T)")) %>%
  
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 2.5, pch = 21) +
  
  facet_wrap(~rarity_2, nrow = 2, ncol = 2) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  labs(fill = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(text = element_text(size = 13)) +
  theme(legend.position = "right") 

## ggsave("figures/final/Fig3_DRR_v_PDRR.tiff", width = 6, height = 5.5)
rc_lo = edge_RR_cats_low %>%
  mutate(rarity_2 = ifelse(rarity_cat == "Tmp Rare, Sp Common", 
                           "Common (S), Intermittent (T)",
                           ifelse(rarity_cat == "Tmp Rare, Sp Rare", 
                                  "Sparse (S), Intermittent (T)", 
                                  ifelse(rarity_cat == "Tmp Common, Sp Common", 
                                         "Common (S), Persistent (T)", 
                                         "Sparse (S), Persistent (T)"))), 
         
         rarity_2 = fct_relevel(rarity_2, "Common (S), Persistent (T)", 
                                "Sparse (S), Persistent (T)",  
                                "Common (S), Intermittent (T)", 
                                "Sparse (S), Intermittent (T)")) %>%
  
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1.5, pch = 21) +
  
  facet_wrap(~rarity_2, nrow = 2, ncol = 2) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  labs(fill = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = "right") +
  ggtitle("Narrow Definition of Common, Persistent") +
  theme(plot.title = element_text(size = 10))

rc_hi = edge_RR_cats_hi %>%
  mutate(rarity_2 = ifelse(rarity_cat == "Tmp Rare, Sp Common", 
                           "Common (S), Intermittent (T)",
                           ifelse(rarity_cat == "Tmp Rare, Sp Rare", 
                                  "Sparse (S), Intermittent (T)", 
                                  ifelse(rarity_cat == "Tmp Common, Sp Common", 
                                         "Common (S), Persistent (T)", 
                                         "Sparse (S), Persistent (T)"))), 
         
         rarity_2 = fct_relevel(rarity_2, "Common (S), Persistent (T)", 
                                "Sparse (S), Persistent (T)",  
                                "Common (S), Intermittent (T)", 
                                "Sparse (S), Intermittent (T)")) %>%
  
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1.5, pch = 21) +
  
  facet_wrap(~rarity_2, nrow = 2, ncol = 2) +
  xlab("Drought Response Ratio") +
  ylab(" ") +
  labs(fill = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = "right") +
  ggtitle("Broad Definition of Common, Persistent") +
  theme(plot.title = element_text(size = 10))

ggarrange(rc_lo, rc_hi, ncol = 2, labels = c("(a)", "(b)"))

ggsave("figures/review_figs/FigSXXX_DRR_v_PDRR_sensitivity_analysis.tiff", 
       width = 18, height = 10, unit = "cm")

