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


# Figure S1 ####
ggplot(edge_FG, aes(x=percrank, y=persistence.site, color = site))+
  geom_point() +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  geom_abline(slope = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity")+
  ylab("Temporal Rarity") +
  labs(color = "Site") 

ggsave("preliminary_figs/oct_2024/figure3.png", width = 6, height = 5.5)

## check correlations
cor(edge_FG[edge_FG$site == "KNZ",]$percrank, edge_FG[edge_FG$site == "KNZ",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "HYS",]$percrank, edge_FG[edge_FG$site == "HYS",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "CHY",]$percrank, edge_FG[edge_FG$site == "CHY",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "SGS",]$percrank, edge_FG[edge_FG$site == "SGS",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "SBL",]$percrank, edge_FG[edge_FG$site == "SBL",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "SBK",]$percrank, edge_FG[edge_FG$site == "SBK",]$persistence.site, method = c("pearson"))

# Figure 2 ####
rankD3 <- ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab("Drought Response Ratio") +
  labs(color = "Site") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15))

rankR3 <- ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Spatial Rarity") +
  ylab("Post-drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15))

persD3 <- ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab(" ") +
  ylab(" ") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15))

persR3 <- ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(alpha = 0.9, size = 0.9, color = "grey") +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 1.35) +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Temporal Rarity") +
  ylab("") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 15))

ggarrange(rankD3, persD3, rankR3, persR3, 
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 2, nrow=2)

ggsave("preliminary_figs/oct_2024/Figure2_RR_drought_recov.png", width = 10, height = 8.5)

## explore site level patterns ####
p1 = ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(aes(color = site), size = 2) +
  geom_smooth(method = "lm", alpha = 0, linewidth = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Rank") +
  ylab("Drought Resp. Ratio") +
  theme(text = element_text(size = 13.5)) +
  facet_wrap(~site, ncol = 6, nrow = 1)

p2 = ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(aes(color = site), size = 2) +
  geom_smooth(method = "lm", alpha = 0, linewidth = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Persistence") +
  ylab("Drought Resp. Ratio") +
  theme(text = element_text(size = 13.5)) +
  facet_wrap(~site, ncol = 6, nrow = 1)

p3 = ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(aes(color = site), size = 2) +
  geom_smooth(method = "lm", alpha = 0, linewidth = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Rank") +
  ylab("Recovery Resp. Ratio") +
  theme(text = element_text(size = 13.5)) +
  facet_wrap(~site, ncol = 6, nrow = 1)

p4 = ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(aes(color = site), size = 2) +
  geom_smooth(method = "lm", alpha = 0, linewidth = 1, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Persistence") +
  ylab("Recovery Resp. Ratio") +
  theme(text = element_text(size = 13.5)) +
  facet_wrap(~site, ncol = 6, nrow = 1)

ggarrange(p1, p2, p3, p4, ncol = 1, nrow = 4, common.legend = T, legend = "bottom")

ggsave("preliminary_figs/june_2024/site_level_responses_large.png", width = 10, height = 8)


# Figure 4 ####
ggplot(edge_FG, aes(x=drought.RR, y=recovery.RR, color = percrank)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(size = 2)+
  facet_wrap(~site, nrow = 3, ncol = 2) +
  xlab("Drought Response Ratio") +
  ylab("Post-drought Response Ratio") +
  scale_color_gradientn(
    colors = c("#E3B710", "#DCCB4E", "#BDC881", "#A2A475", "#81A88D", "#00A08A", "#0B775E", "#175149")) +
  labs(color = "Spatial Rarity") +
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

ggsave("preliminary_figs/oct_2024/figure4.png", width = 7, height = 6)
  
 
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
