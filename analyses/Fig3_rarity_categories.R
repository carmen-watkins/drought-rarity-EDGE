# Header ####
## Script name: Categorization sensitivity analysis
##
##' Purpose of script: Explore whether moving the bounds of rarity categories
##' changes the findings from Fig 3
##
## Author: Carmen Watkins
##
library(tidyverse)

# Set up ####
source("analyses/calc_response_ratio.R") 

library(ggpubr)

## set up graphics
theme_set(theme_classic())

# Define Rarity Categories ####
## Standard Def ####
## as in figure 3
edge_RR_cats = edge_RR %>%
  mutate(spatial = ifelse(spatial_rarity < 0.25, "Sp Common", "Sp Rare"),
         temporal = ifelse(temporal_rarity < 0.5, "Tmp Common", "Tmp Rare"),
         rarity_cat = paste0(temporal, ", ", spatial),
         rarity_2 = ifelse(rarity_cat == "Tmp Rare, Sp Common", 
                           "Common (S), Intermittent (T)",
                           ifelse(rarity_cat == "Tmp Rare, Sp Rare", 
                                  "Sparse (S), Intermittent (T)", 
                                  ifelse(rarity_cat == "Tmp Common, Sp Common", 
                                         "Common (S), Persistent (T)", 
                                         "Sparse (S), Persistent (T)"))), 
         
         rarity_2 = fct_relevel(rarity_2, "Common (S), Persistent (T)", 
                                "Sparse (S), Persistent (T)",  
                                "Common (S), Intermittent (T)", 
                                "Sparse (S), Intermittent (T)")) 

## calc correlations 
norm_corr = edge_RR_cats %>%
  filter(!is.na(resp.ratio.site_D4), !is.na(resp.ratio.site_PDfull)) %>%
  group_by(rarity_2) %>%
  summarise(corr_RR = cor(resp.ratio.site_D4, resp.ratio.site_PDfull),
            corr_RR = round(corr_RR, 3)) %>%
  mutate(x = c(-0.8, -0.8, -0.8, -0.8), ## x & y values where label should be placed
         y = c(1.15, 1.15, 1.15, 1.15))

## Narrow Def ####
## narrow definition of common/persistent species
edge_RR_cats_narrow = edge_RR %>%
  mutate(spatial = ifelse(spatial_rarity < 0.15, "Sp Common", "Sp Rare"),
         temporal = ifelse(temporal_rarity < 0.4, "Tmp Common", "Tmp Rare"),
         rarity_cat = paste0(temporal, ", ", spatial),
         rarity_2 = ifelse(rarity_cat == "Tmp Rare, Sp Common", 
                           "Common (S), Intermittent (T)",
                           ifelse(rarity_cat == "Tmp Rare, Sp Rare", 
                                  "Sparse (S), Intermittent (T)", 
                                  ifelse(rarity_cat == "Tmp Common, Sp Common", 
                                         "Common (S), Persistent (T)", 
                                         "Sparse (S), Persistent (T)"))), 
         
         rarity_2 = fct_relevel(rarity_2, "Common (S), Persistent (T)", 
                                "Sparse (S), Persistent (T)",  
                                "Common (S), Intermittent (T)", 
                                "Sparse (S), Intermittent (T)"))

## calc correlations 
narrow_corr = edge_RR_cats_narrow %>%
  filter(!is.na(resp.ratio.site_D4), !is.na(resp.ratio.site_PDfull)) %>%
  group_by(rarity_2) %>%
  summarise(corr_RR = cor(resp.ratio.site_D4, resp.ratio.site_PDfull),
            corr_RR = round(corr_RR, 3)) %>%
  mutate(x = c(-0.8, -0.8, -0.8, -0.8),
         y = rep(1.15, 4))

## Broad Def ####
## broad definition of common/persistent species
edge_RR_cats_broad = edge_RR %>%
  mutate(spatial = ifelse(spatial_rarity < 0.35, "Sp Common", "Sp Rare"),
         temporal = ifelse(temporal_rarity < 0.6, "Tmp Common", "Tmp Rare"),
         rarity_cat = paste0(temporal, ", ", spatial),
         rarity_2 = ifelse(rarity_cat == "Tmp Rare, Sp Common", 
                           "Common (S), Intermittent (T)",
                           ifelse(rarity_cat == "Tmp Rare, Sp Rare", 
                                  "Sparse (S), Intermittent (T)", 
                                  ifelse(rarity_cat == "Tmp Common, Sp Common", 
                                         "Common (S), Persistent (T)", 
                                         "Sparse (S), Persistent (T)"))), 
         
         rarity_2 = fct_relevel(rarity_2, "Common (S), Persistent (T)", 
                                "Sparse (S), Persistent (T)",  
                                "Common (S), Intermittent (T)", 
                                "Sparse (S), Intermittent (T)"))

## calc correlations 
broad_corr = edge_RR_cats_broad %>%
  filter(!is.na(resp.ratio.site_D4), !is.na(resp.ratio.site_PDfull)) %>%
  group_by(rarity_2) %>%
  summarise(corr_RR = cor(resp.ratio.site_D4, resp.ratio.site_PDfull),
            corr_RR = round(corr_RR, 3))  %>%
  mutate(x = c(-0.8, -0.8, -0.8, -0.8),
         y = rep(1.15, 4))


## arrange sites
edge_RR_cats$site = factor(edge_RR_cats$site, levels = c("KNZ", "HYS", "CHY", 
                                                         "SGS", "SBL", "SBK"))

edge_RR_cats_narrow$site = factor(edge_RR_cats_narrow$site, 
                               levels = c("KNZ", "HYS", "CHY", "SGS", 
                                          "SBL", "SBK"))

edge_RR_cats_broad$site = factor(edge_RR_cats_broad$site, 
                              levels = c("KNZ", "HYS", "CHY", "SGS", 
                                         "SBL", "SBK"))

# Figure 3 ####
edge_RR_cats %>%
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
  theme(legend.position = "right") +
  geom_text(data = norm_corr, mapping = aes(x = x, y = y, label = corr_RR))

## save official version
## ggsave("figures/review_figs/Fig3_DRR_v_PDRR.tiff", width = 15, height = 14, 
   #    units = "cm")

## save review version
ggsave("figures/review_figs/Fig3_DRR_v_PDRR.png", width = 15, height = 14, 
           units = "cm")
       
# Figure S11 ####
rc_lo = edge_RR_cats_narrow %>%
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
  theme(plot.title = element_text(size = 10)) +
  geom_text(data = narrow_corr, mapping = aes(x = x, y = y, label = corr_RR),
            size = 3)

rc_hi = edge_RR_cats_broad %>%
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
  theme(plot.title = element_text(size = 10)) +
  geom_text(data = broad_corr, mapping = aes(x = x, y = y, label = corr_RR),
            size = 3)

ggarrange(rc_lo, rc_hi, ncol = 2, labels = c("a", "b"))

ggsave("figures/review_figs/supp/FigS11_DRR_v_PDRR_sensitivity_analysis.png", 
       width = 18, height = 10, unit = "cm")



# summarise categories ####
CIsp = edge_RR_cats %>%
  filter(rarity_cat == "Tmp Rare, Sp Common")

## get number of samples in Common, Intermittent category
cats_sum = edge_RR_cats %>%
  group_by(rarity_cat) %>%
  summarise(numcat = n())

CI_cat = edge_RR_cats %>%
  mutate(rarity_2 = ifelse(rarity_cat == "Tmp Rare, Sp Common", 
                           "Common (S), Intermittent (T)",
                           ifelse(rarity_cat == "Tmp Rare, Sp Rare", 
                                  "Sparse (S), Intermittent (T)", 
                                  ifelse(rarity_cat == "Tmp Common, Sp Common", 
                                         "Common (S), Persistent (T)", 
                                         "Sparse (S), Persistent (T)")))) %>%
  group_by(rarity_cat)


## write.csv(edge_RR_cats, "../edge_RR_and_rarity_categories.csv")
