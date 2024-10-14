# Header ####
## Script name: Response Ratio Analyses

## Purpose of script: Visualize drought and post-drought response ratios in relation to rarity patterns.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
source("analyses/calculate_response_ratio.R") 
source("analyses/color_palettes.R")

library(viridisLite)
library(scico)
library(cowplot)
library(ggpubr)
library(viridis)
library(ggExtra)
#library(wesanderson)

## set up graphics
theme_set(theme_classic())
pal <- wes_palette("Royal3")
wes_palette("Royal3")

## get number of unique species in analyses
unique(edge_FG$species)
## 289

## arrange sites in df
edge_FG$site <- factor(edge_FG$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

## Summary Stats ####
sumRR <- edge_FG %>% 
  mutate(rarity = ifelse(percrank > 0.98, "Dom", "Sub")) %>%
  mutate(quad = ifelse(drought.RR > 0 & recovery.RR > 0, "+D+R",
                       ifelse(drought.RR > 0 & recovery.RR < 0, "+D-R",
                              ifelse(drought.RR < 0 & recovery.RR < 0, "-D-R",
                                     ifelse(drought.RR < 0 & recovery.RR > 0,"-D+R", 
                                            ifelse(drought.RR == 0 & recovery.RR != 0, "recov",
                                                   ifelse(drought.RR != 0 & recovery.RR == 0, "lost", 
                                                          ifelse(drought.RR == 0 & recovery.RR == 0, "no fx", NA)))))))) %>%
  group_by(site, quad, rarity) %>%
  summarise(quad.tot = n()) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(overall.tot= sum(quad.tot),
         percent.quad = quad.tot/overall.tot)


sumRR$quad <- factor(sumRR$quad, levels = c("+D+R", "+D-R", "-D-R", "-D+R", "lost", "recov", "no fx"))

## Categorize species ####
edge_FG_cats = edge_FG %>%
  mutate(spatial = ifelse(percrank > 0.5, "Abundant", "Scarce"),
         temporal = ifelse(persistence.site > 0.5, "Core", "Transient"),
         rarity_cat = paste0(temporal, ", ", spatial)) 


category_sums = edge_FG_cats %>%
  group_by(site, rarity_cat) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(tot = sum(num)) %>%
  ungroup() %>%
  mutate(perc = num/tot)

# Figure 1 ####
ggplot(edge_FG_cats, aes(x=percrank, y=persistence.site, color = rarity_cat))+
  scale_color_manual(values = c("#5D69B1", "#CC61B0", "#99C945","#E58606")) +
  geom_point(size = 2.5, alpha = 0.65) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
 # xlab("Spatial Rarity")+
 # ylab("Temporal Rarity") +
  xlab(NULL) +
  ylab(NULL) +
  labs(color = "Rarity") +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 0.5) +
  annotate("text", x = 0.15, y = 1.05, label = "Core, Scarce", size = 4) +
  annotate("text", x = 0.15, y = -0.05, label = "Transient, Scarce", size = 4) +
  annotate("text", x = 0.75, y = 1.05, label = "Core, Abundant", size = 4) +
  annotate("text", x = 0.75, y = -0.05, label = "Transient, Abundant", size = 4) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 13)) +
  scale_x_reverse() +
  scale_y_reverse()

ggsave("preliminary_figs/oct_2024/post_lab_feedback/figure1.tiff", width = 4.2, height = 4)


# Figure 2 ####


## panel B
str(edge_FG_cats)
edge_FG_cats$rarity_cat = as.factor(edge_FG_cats$rarity_cat)
edge_FG_cats = edge_FG_cats %>%
  mutate(rarity_cat = fct_relevel(rarity_cat, "Core, Scarce", "Core, Abundant", "Transient, Scarce", "Transient, Abundant"))

p1 = ggplot(edge_FG_cats, aes(x=drought.RR, y=recovery.RR, color = site)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2.5)+
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
 # scale_color_manual(values = c("#CC61B0", "#5D69B1","#E58606", "#99C945")) +
  scale_color_manual(values = pal) +
  labs(color = "Rarity") +
  theme_bw() +
  geom_smooth(method = "lm", alpha = .1, linewidth = 2) +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(ylim = c(-1.22, 1.22)) +
  annotate(geom="text", x=-0.75, y=1.2, label="low resist, high recov",
           color="black", size = 2.5) +
  annotate(geom="text", x=-0.75, y=-1.2, label="low resist, low recov",
           color="black", size = 2.5) +
  annotate(geom="text", x=0.75, y=1.2, label="high resist, high recov",
           color="black", size = 2.5) +
  annotate(geom="text", x=0.75, y=-1.2, label="high resist, low recov",
           color="black", size = 2.5) +
  theme(text = element_text(size = 13)) +
  theme(legend.position = "bottom") +
  facet_wrap(~rarity_cat)

ggMarginal(p1, groupColour = TRUE)

## panel C
ggplot(edge_FG_cats, aes(x=percrank, y=persistence.site, color = rarity_cat))+
  geom_point(size = 1.5) +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  scale_color_manual(values = c("#5D69B1", "#CC61B0", "#99C945","#E58606")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 0.5) +
  theme(legend.position = "right") +
  theme(text = element_text(size = 13)) +
  labs(color = "Rarity Category")  +
  scale_x_reverse() +
  scale_y_reverse()

## put panels together
#pt1 = plot_grid(a, b, ncol = 2, rel_widths = c(0.65,1))

#plot_grid(pt1, c, ncol = 1, rel_heights = c(2,1))

ggsave("preliminary_figs/oct_2024/post_lab_feedback/figure2.tiff", width = 8, height = 7)

## category summaries ####
## double check code before using these


## check correlations ####
cor(edge_FG[edge_FG$site == "KNZ",]$percrank, edge_FG[edge_FG$site == "KNZ",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "HYS",]$percrank, edge_FG[edge_FG$site == "HYS",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "CHY",]$percrank, edge_FG[edge_FG$site == "CHY",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "SGS",]$percrank, edge_FG[edge_FG$site == "SGS",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "SBL",]$percrank, edge_FG[edge_FG$site == "SBL",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "SBK",]$percrank, edge_FG[edge_FG$site == "SBK",]$persistence.site, method = c("pearson"))


# Figure 3 ####
rankD3 <- ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

rankR3 <- ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persD3 <- ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

persR3 <- ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0.05, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15)) +
  scale_x_reverse()

ggarrange(rankD3, persD3, rankR3, persR3, 
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 2, nrow=2)

ggsave("preliminary_figs/oct_2024/post_lab_feedback/fig3_site_RR_diffs.png", width = 10, height = 8.5)


# Figure 4 ####
ggplot(edge_FG_cats, aes(x=drought.RR, y=recovery.RR, color = rarity_cat)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2.5)+
  facet_wrap(~site, nrow = 3, ncol = 2) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  scale_color_manual(values = c("#5D69B1", "#CC61B0", "#99C945","#E58606")) +
  labs(color = "Rarity") +
  theme_bw() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(ylim = c(-1.22, 1.22)) +
  annotate(geom="text", x=-0.75, y=1.2, label="low resistance, high recovery",
           color="black", size = 3.5) +
  annotate(geom="text", x=-0.75, y=-1.2, label="low resistance, low recovery",
           color="black", size = 3.5) +
  annotate(geom="text", x=0.75, y=1.2, label="high resistance, high recovery",
           color="black", size = 3.5) +
  annotate(geom="text", x=0.75, y=-1.2, label="high resistance, low recovery",
           color="black", size = 3.5) +
  theme(text = element_text(size = 15))

ggsave("preliminary_figs/oct_2024/figure4_color_cats.png", width = 7, height = 6)

## fig 4 alt view ####  
ggplot(edge_FG_cats, aes(x=drought.RR, y=recovery.RR, color = site)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2)+
  facet_wrap(~rarity_cat, nrow = 3, ncol = 2) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  #scale_color_manual(values = c("#5D69B1", "#52BCA3", "#99C945","#E58606")) +
  labs(color = "Site") +
  geom_smooth(method = "lm", alpha = 0.02)+
  scale_color_manual(values = pal) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(ylim = c(-1.25, 1.25
  )) +
  annotate(geom="text", x=-0.75, y=1.2, label="-D+R",
           color="black", size = 3) +
  annotate(geom="text", x=-0.75, y=-1.2, label="-D-R",
           color="black", size = 3) +
  annotate(geom="text", x=0.75, y=1.2, label="+D+R",
           color="black", size = 3) +
  annotate(geom="text", x=0.75, y=-1.2, label="+D-R",
           color="black", size = 3) +
  theme(text = element_text(size = 15))
#sumRR2 = sumRR %>%
 # mutate(position_x = ifelse(substr(quad, start = 1, stop = 2) == "+D", 0.25, -0.25),
        # position_y = ifelse(substr(quad, start = 3, stop = 4) == "+R", 0.25, -0.25),
        # position_x = ifelse(quad == "no fx", 0, position_x),
         #position_y = ifelse(quad == "no fx", 0, position_y),
        # position_y = ifelse(quad == "lost", -0.5, position_y),
        # position_x = ifelse(quad == "lost", 0, position_x),
       #  position_y = ifelse(quad == "recov", 0.5, position_y),
       # position_x = ifelse(quad == "recov", 0, position_x), 
         
       #  position_y = ifelse(rarity == "Dom", position_y - 0.1, position_y),
       #  position_x = ifelse(rarity == "Dom", position_x - 0.15, position_x))

#pb = ggplot(sumRR2, aes(x=position_x, y=position_y)) +
 # geom_hline(yintercept = 0) +
 # geom_vline(xintercept = 0) +
 # geom_point(aes(fill=rarity, size = percent.quad*10), 
       #       colour="black",pch=21) +
#  scale_fill_manual(values = c("white", "black"))+
#  facet_wrap(~site, ncol = 1, nrow = 6) +
 # theme_bw() +
#  theme(panel.grid = element_blank()) +
 # theme(strip.background =element_rect(fill="white")) +
#  coord_cartesian(xlim = c(-0.6,0.6), ylim = c(-0.6,0.6)) +
#  xlab("Drought Response Ratio") + ylab("") +
 # theme(text = element_text(size = 15)) +
 # labs(size = "Proportion", fill = "Abundance") +
  #geom_text(aes(label = round(percent.quad, digits = 3)), check_overlap = TRUE, hjust = "outward", size = 4, vjust = "top") +
 # theme(legend.position="bottom")

#ggarrange(pa, pb, ncol = 2, nrow = 1)

# Figure S5 ####
## Persist by Site ####
ggplot(edge_FG, aes(x=drought.RR, y=recovery.RR, color = persistence.site)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 1, ncol = 6) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_gradientn(
    colors = c("#E3B710", "#DCCB4E", "#BDC881", "#A2A475", "#81A88D", "#00A08A", "#0B775E", "#175149")) +
  labs(color = "Persistence")  +
  theme(legend.position="bottom") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white"))

ggsave("preliminary_figs/june_2024/figureS5.tiff", width = 9, height = 2)

# Old Fig Versions ####
## keep for posterity
## Fig 1 attempt ####
edge_FG_long = edge_FG %>%
  pivot_longer(cols = c("persistence.site", "percrank"), names_to = "rarity_type", values_to = "rarity_score") %>%
  mutate(rarity_type = ifelse(rarity_type == "persistence.site", "Temporal", "Spatial"))

d = ggplot(edge_FG_long, aes(x=rarity_score, y=drought.RR)) +
  geom_point(aes(fill=rarity_type), 
             colour="black",pch=21, size=1.5, alpha = 0.25) + 
  scale_fill_manual(values = c("black", "white")) +
  geom_smooth(method = "lm", alpha = 0.1, linewidth = 2, aes(color = rarity_type)) +
  scale_color_manual(values = c("#202020", "#a3a3a3")) +
  xlab("Rarity") + 
  ylab("Drought Response Ratio") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #scale_color_manual(values = c("#E58606", "#5D69B1"))+
  scale_shape_manual(values = c(19, 1)) +
  labs(fill = "Rarity Type", color = "Rarity Type") +
  theme(text = element_text(size = 15))

pd = ggplot(edge_FG_long, aes(x=rarity_score, y=recovery.RR)) +
  geom_point(aes(fill=rarity_type), 
             colour="black",pch=21, size=1.5, alpha = 0.25) + 
  scale_fill_manual(values = c("black", "white")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", alpha = 0.1, linewidth = 2, aes(color = rarity_type)) +
  scale_color_manual(values = c("#080808", "#a3a3a3")) +
  xlab("Rarity") + 
  ylab("Post-Drought Response Ratio") +
  
  #scale_color_manual(values = c("#E58606", "#5D69B1"))+
  scale_shape_manual(values = c(19, 1)) +
  labs(fill = "Rarity Type", color = "Rarity Type") +
  theme(text = element_text(size = 15))

ggarrange(d, pd, common.legend = TRUE, labels = "AUTO", legend = "bottom")
ggsave("preliminary_figs/oct_2024/fig2rarity_responses_nosite.tiff", width = 8, height = 4.5)

