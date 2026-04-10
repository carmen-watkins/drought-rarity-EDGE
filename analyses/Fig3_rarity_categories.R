# Header ####
## Script name: Categorization sensitivity analysis
##
##' Purpose of script: Explore whether moving the bounds of rarity categories
##' changes the findings from Fig 3
##
## Author: Carmen Watkins
##

# Set up ####
source("analyses/calc_response_ratio.R") 

## load packages
library(tidyverse)
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
         y = rep(1.2, 4))

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
         y = rep(1.2, 4))


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
#plot = edge_RR_cats %>%
 # ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  #geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
#  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
 # geom_point(fill = "grey", colour = "black", size = 2.5, pch = 21) +
  #facet_wrap(~rarity_2, nrow = 2, ncol = 2) +
#  xlab("Drought Response Ratio") +
 # ylab("Post-drought Response Ratio") +
  #labs(fill = NULL) +
#  theme_bw() +
 # theme(panel.grid = element_blank()) +
  #theme(strip.background =element_rect(fill="white")) +
#  theme(text = element_text(size = 13)) +
 # theme(legend.position = "right") +
  #geom_text(data = norm_corr, mapping = aes(x = x, y = y, label = corr_RR))

## panel version ####
a = edge_RR_cats %>%
  filter(rarity_2 == "Common (S), Persistent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1.5, pch = 21) +
  xlab(" ") +
  ylab("Post-drought Response Ratio") +
  ggtitle("Common, Persistent") +
  theme(text = element_text(size = 13)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 12)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.2)) +
  geom_text(data = norm_corr[norm_corr$rarity_2 == "Common (S), Persistent (T)",], mapping = aes(x = x, y = y, label = corr_RR))

b = edge_RR_cats %>%
  filter(rarity_2 == "Sparse (S), Persistent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1.5, pch = 21) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sparse, Persistent") +
  theme(text = element_text(size = 13)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 12)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.2)) +
  geom_text(data = norm_corr[norm_corr$rarity_2 == "Sparse (S), Persistent (T)",], mapping = aes(x = x, y = y, label = corr_RR))

c = edge_RR_cats %>%
  filter(rarity_2 == "Common (S), Intermittent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1.5, pch = 21) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  ggtitle("Common, Intermittent") +
  theme(text = element_text(size = 13)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 12)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.2)) +
  geom_text(data = norm_corr[norm_corr$rarity_2 == "Common (S), Intermittent (T)",], mapping = aes(x = x, y = y, label = corr_RR))

d = edge_RR_cats %>%
  filter(rarity_2 == "Sparse (S), Intermittent (T)") %>%
  
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1.5, pch = 21) +
  xlab("Drought Response Ratio") +
  ylab(" ") +
  ggtitle("Sparse, Intermittent") +
  theme(text = element_text(size = 13)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 12)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.2)) +
  geom_text(data = norm_corr[norm_corr$rarity_2 == "Sparse (S), Intermittent (T)",], mapping = aes(x = x, y = y, label = corr_RR))

plot = ggarrange(a, b, c, d, labels = "auto")

annotate_figure(plot, top = text_grob("Figure 3", 
                                      color = "black", face = "bold", size = 14))

## save official version
ggsave("figures/final_figs/Fig3_DRR_v_PDRR.tiff", width = 13, height = 13.75, 
      units = "cm")
       
# Figure S11 ####
## narrow ####
### panel version ####
al = edge_RR_cats_narrow %>%
  filter(rarity_2 == "Common (S), Persistent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1, pch = 21) +
  xlab(" ") +
  ylab("Post-drought RR") +
  ggtitle("Common, Persistent") +
  theme(text = element_text(size = 11)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 11)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.25)) +
  geom_text(data = narrow_corr[narrow_corr$rarity_2 == "Common (S), Persistent (T)",], 
            mapping = aes(x = x, y = y, label = corr_RR))

bl = edge_RR_cats_narrow %>%
  filter(rarity_2 == "Sparse (S), Persistent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1, pch = 21) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sparse, Persistent") +
  theme(text = element_text(size = 11)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 11)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.25)) +
  geom_text(data = narrow_corr[narrow_corr$rarity_2 == "Sparse (S), Persistent (T)",],
            mapping = aes(x = x, y = y, label = corr_RR))

cl = edge_RR_cats_narrow %>%
  filter(rarity_2 == "Common (S), Intermittent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1, pch = 21) +
  xlab("Drought RR") +
  ylab("Post-drought RR") +
  ggtitle("Common, Intermittent") +
  theme(text = element_text(size = 11)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 11)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.25)) +
  geom_text(data = narrow_corr[narrow_corr$rarity_2 == "Common (S), Intermittent (T)",], 
            mapping = aes(x = x, y = y, label = corr_RR))

dl = edge_RR_cats_narrow %>%
  filter(rarity_2 == "Sparse (S), Intermittent (T)") %>%
  
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1, pch = 21) +
  xlab("Drought RR") +
  ylab(" ") +
  ggtitle("Sparse, Intermittent") +
  theme(text = element_text(size = 11)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 11)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.25)) +
  geom_text(data = narrow_corr[narrow_corr$rarity_2 == "Sparse (S), Intermittent (T)",],
            mapping = aes(x = x, y = y, label = corr_RR))

narrow = ggarrange(al, bl, cl, dl, labels = "auto")

## broad ####
### panel version ####
ab = edge_RR_cats_broad %>%
  filter(rarity_2 == "Common (S), Persistent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1, pch = 21) +
  xlab(" ") +
  ylab("Post-drought RR") +
  ggtitle("Common, Persistent") +
  theme(text = element_text(size = 11)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 11)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.25)) +
  geom_text(data = broad_corr[broad_corr$rarity_2 == "Common (S), Persistent (T)",], 
            mapping = aes(x = x, y = y, label = corr_RR))

bb = edge_RR_cats_broad %>%
  filter(rarity_2 == "Sparse (S), Persistent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1, pch = 21) +
  xlab(" ") +
  ylab(" ") +
  ggtitle("Sparse, Persistent") +
  theme(text = element_text(size = 11)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 11)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.25)) +
  geom_text(data = broad_corr[broad_corr$rarity_2 == "Sparse (S), Persistent (T)",],
            mapping = aes(x = x, y = y, label = corr_RR))

cb = edge_RR_cats_broad %>%
  filter(rarity_2 == "Common (S), Intermittent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1, pch = 21) +
  xlab("Drought RR") +
  ylab("Post-drought RR") +
  ggtitle("Common, Intermittent") +
  theme(text = element_text(size = 11)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 11)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.25)) +
  geom_text(data = broad_corr[broad_corr$rarity_2 == "Common (S), Intermittent (T)",], 
            mapping = aes(x = x, y = y, label = corr_RR))

db = edge_RR_cats_broad %>%
  filter(rarity_2 == "Sparse (S), Intermittent (T)") %>%
  ggplot(aes(x=resp.ratio.site_D4, y=resp.ratio.site_PDfull)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(fill = "grey", colour = "black", size = 1, pch = 21) +
  xlab("Drought RR") +
  ylab(" ") +
  ggtitle("Sparse, Intermittent") +
  theme(text = element_text(size = 11)) +
  theme(axis.title=element_text(size=11), 
        plot.title = element_text(size = 11)) +
  coord_cartesian(xlim = c(-1,1), ylim = c(-1,1.25)) +
  geom_text(data = broad_corr[broad_corr$rarity_2 == "Sparse (S), Intermittent (T)",],
            mapping = aes(x = x, y = y, label = corr_RR))

broad = ggarrange(ab, bb, cb, db, labels = c("e", "f", "g", "h"))


narrow_title = annotate_figure(narrow, top = text_grob("Narrow Definition of Common, Persistent Species", 
                                      color = "black", size = 12))

broad_title = annotate_figure(broad, top = text_grob("Broad Definition of Common, Persistent Species", 
                                                       color = "black", size = 12))

ggarrange(narrow_title, broad_title, nrow = 2, ncol = 1)

ggsave("figures/final_figs/supp_figs/FigS11_DRR_v_PDRR_sensitivity_analysis.png", 
       width = 12, height = 21, unit = "cm")

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
