# Header ####
## Script name: Response Ratio Functional Group

## Purpose of script: Visualize drought and recovery response ratios in relation to functional group and rarity patterns.
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
ggplot(edge_FG, aes(x=percrank, y=persistence.site))+
  geom_point() +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  geom_abline(slope = 1) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  xlab("Percent Rank")+
  ylab("Persistence")

ggsave("preliminary_figs/june_2024/figureS1.png", width = 10, height = 2)

## check correlations
cor(edge_FG[edge_FG$site == "KNZ",]$percrank, edge_FG[edge_FG$site == "KNZ",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "HYS",]$percrank, edge_FG[edge_FG$site == "HYS",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "CHY",]$percrank, edge_FG[edge_FG$site == "CHY",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "SGS",]$percrank, edge_FG[edge_FG$site == "SGS",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "SBL",]$percrank, edge_FG[edge_FG$site == "SBL",]$persistence.site, method = c("pearson"))
cor(edge_FG[edge_FG$site == "SBK",]$percrank, edge_FG[edge_FG$site == "SBK",]$persistence.site, method = c("pearson"))

# Figure 3 ####
## no color
rankD2 <- ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(color = "#9F9F9F") +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Rank") +
  ylab("Drought Resp. Ratio") +
  theme(text = element_text(size = 13.5))

rankR2 <- ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(color = "#9F9F9F")+
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Rank") +
  ylab("Recovery Resp. Ratio") +
  theme(text = element_text(size = 13.5))

persD2 <- ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(color = "#9F9F9F") +
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Persistence") +
  ylab("Drought Resp. Ratio") +
  theme(text = element_text(size = 13.5))

persR2 <- ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(color = "#9F9F9F")+
  geom_smooth(method = "lm", alpha = 0.25, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Persistence") +
  ylab("Recovery Resp. Ratio") +
  theme(text = element_text(size = 13.5))

rankD3 <- ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(aes(color = site), alpha = 0.75, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Rank") +
  ylab("Drought Resp. Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13.5))

rankR3 <- ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(aes(color = site), alpha = 0.75, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Rank") +
  ylab("Recovery Resp. Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13.5))

persD3 <- ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(aes(color = site), alpha = 0.75, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Persistence") +
  ylab("Drought Resp. Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13.5))

persR3 <- ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(aes(color = site), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = pal) +
  xlab("Persistence") +
  ylab("Recovery Resp. Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(text = element_text(size = 13.5))

ggarrange(rankD2, persD2, rankR2, persR2, 
          rankD3, persD3, rankR3, persR3, 
          labels = "AUTO", common.legend = T, legend = "bottom", ncol = 4, nrow=2)

ggsave("preliminary_figs/march_2024/Figure4_RR_drought_recov.png", width = 10, height = 6)

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
pa <- ggplot(edge_FG, aes(x=drought.RR, y=recovery.RR, color = percrank)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point()+
  facet_wrap(~site, nrow = 1, ncol = 6) +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_gradientn(
    colors = c("#E3B710", "#DCCB4E", "#BDC881", "#A2A475", "#81A88D", "#00A08A", "#0B775E", "#175149")) +
  labs(color = "Rank") +
  
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
           color="black", size = 3)
  
#ggsave("preliminary_figs/june_2024/figure4.tiff", width = 9, height = 2)

pb <- ggplot(sumRR, aes(x=quad, y=percent.quad, fill = rarity)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~site, ncol = 6, nrow = 1) +
  xlab("Response Trajectory") + ylab("Proportion of Species") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values = c("#175149", "#BDC881"), name = NULL) +
  theme(legend.position="right") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

ggarrange(pa, pb, ncol = 1, nrow = 2)

ggsave("preliminary_figs/june_2024/figure4.png", height = 5, width = 10)

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
