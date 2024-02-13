# Header ####
## Script name: Response Ratio Functional Group

## Purpose of script: Visualize drought and recovery response ratios in relation to functional group and rarity patterns.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up ####
source("analyses/calculate_response_ratio.R") 

library(viridisLite)
library(scico)
library(cowplot)
library(ggpubr)

theme_set(theme_classic())

# Figure 1 ####
rankD <- ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(aes(color = site), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

rankR <- ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(aes(color = site), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Rank") +
  ylab("Recovery Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

persD <- ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(aes(color = site), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

persR <- ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(aes(color = site), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = site), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#008080", "#70a494", "#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

ggarrange(rankD, persD, rankR, persR, labels = "AUTO", common.legend = T, legend = "bottom")

ggsave("preliminary_figs/meeting_jan_2024/RR_drought_recov_SITE.png", width = 6.5, height = 6.5)

# Figure 2 ####
## Rank by Site ####
rank_RR_DR <- ggplot(edge_FG, aes(x=drought.RR, y=recovery.RR, color = percrank)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  #scale_color_viridis_c(direction = -1) +
  scale_color_scico(palette = "roma", direction = -1) +
  labs(color = "Rank") +
  theme(legend.position="bottom")

#ggsave("preliminary_figs/meeting_jan_2024/DRR_v_RRR_rank_site.png", width = 5, height = 4.5)

## Persist by Site ####
ggplot(edge_FG, aes(x=drought.RR, y=recovery.RR, color = persistence.site)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_scico(palette = "roma", direction = -1) +
  labs(color = "Persistence")

ggsave("preliminary_figs/meeting_jan_2024/DRR_v_RRR_persist_site.png", width = 5, height = 4.5)

## Summary Stats ####
sumRR <- edge_FG %>% 
  mutate(quad = ifelse(drought.RR > 0 & recovery.RR > 0, "Q2",
                       ifelse(drought.RR > 0 & recovery.RR < 0, "Q4",
                              ifelse(drought.RR < 0 & recovery.RR < 0, "Q3",
                                     ifelse(drought.RR < 0 & recovery.RR > 0,"Q1", 
                                            ifelse(drought.RR == 0 & recovery.RR != 0, "recov",
                                                   ifelse(drought.RR != 0 & recovery.RR == 0, "lost", 
                                                          ifelse(drought.RR == 0 & recovery.RR == 0, "no fx", NA)))))))) %>%
  group_by(site, quad) %>%
  summarise(quad.tot = n()) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(overall.tot= sum(quad.tot),
         percent.quad = quad.tot/overall.tot)

sumRR$quad <- factor(sumRR$quad, levels = c("Q1", "Q2", "Q3", "Q4", "lost", "recov", "no fx"))

prop <- ggplot(sumRR, aes(x=quad, y=percent.quad)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~site, ncol = 1, nrow = 6) +
  xlab("Quadrant") + ylab("% Species of Total") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggarrange(rank_RR_DR, prop)

ggsave("preliminary_figs/meeting_jan_2024/sp_percent_quadrants_site.png", height = 5, width = 5)

plot_grid(rank_RR_DR, prop,
          #align = "v",
          rel_widths = c(3,1), 
          labels = "AUTO")

ggsave("preliminary_figs/meeting_jan_2024/rank_DRR_RRR_props.png", height = 5, width = 6)


# Sp Strategies ####
## FG by Site ####
ggplot(edge_FG, aes(x=drought.RR, y=recovery.RR, color = FunctionalGroup)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606"))

ggsave("preliminary_figs/meeting_jan_2024/DRR_v_RRR_FG_site.png", width = 5, height = 4.5)



## Precip Bins ####
ggplot(edge_FG, aes(x=drought.RR, y= recovery.RR, color = precip.bin)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~precip.bin, nrow = 1, ncol = 3) +
  #geom_smooth(method = "lm", alpha = 0.10, color = "black", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#42B7B9","#ca562c", "#D691C1")) +
  labs(color="Relative PPT Bin")
  
ggsave("preliminary_figs/meeting_jan_2024/DRR_v_RRR_precip_bins.png", width = 7, height = 2.5)

## Precip Facet x FG color ####
ggplot(edge_FG, aes(x=drought.RR, recovery.RR, color = FunctionalGroup)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~precip.bin, nrow = 1, ncol = 3) +
  #geom_smooth(method = "lm", alpha = 0.10, color = "black", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  labs(color="Functional Group")

ggsave("preliminary_figs/meeting_jan_2024/Fig2_DRR_v_RRR_facetppt_colorFG.png", width = 7, height = 2.5)

# Figure 3: Overall FG patterns ####
FGrankD <- ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

FGrankR <- ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Recovery Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

FGpersD <- ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

FGpersR <- ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

ggarrange(FGrankD, FGpersD, FGrankR, FGpersR, labels = "AUTO", common.legend = T, legend = "bottom")

ggsave("preliminary_figs/meeting_jan_2024/RR_drought_recov_FG.png", width = 6.5, height = 6.5)




# Other Analyses ####
## Site Level Patterns ####
ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~site)

ggsave("preliminary_figs/meeting_jan_2024/DR_FG_by_site.png", width = 7, height = 3.5)

ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Recovery Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~site)

ggsave("preliminary_figs/meeting_jan_2024/RR_FG_by_site.png", width = 7, height = 3.5)

ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~site)

ggsave("preliminary_figs/meeting_jan_2024/DP_FG_by_site.png", width = 7, height = 3.5)

ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~site)

ggsave("preliminary_figs/meeting_jan_2024/RP_FG_by_site.png", width = 7, height = 3.5)


## Facet by FG #####
RD <- ggplot(edge_FG, aes(x=percrank, y=drought.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  #geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~FunctionalGroup, ncol = 5)

RR <- ggplot(edge_FG, aes(x=percrank, y=recovery.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  #geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Recovery Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~FunctionalGroup, ncol = 5)

ggarrange(RD, RR, ncol = 1, nrow = 2,common.legend = T, legend = "bottom")

ggsave("preliminary_figs/meeting_jan_2024/RR_rank_FG_facet.png", width = 5.5, height = 4)

PD <- ggplot(edge_FG, aes(x=persistence.site, y=drought.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  #geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~FunctionalGroup, ncol = 5)

PR <- ggplot(edge_FG, aes(x=persistence.site, y=recovery.RR)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  #geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~FunctionalGroup, ncol = 5)

ggarrange(PD, PR, ncol = 1, nrow = 2,common.legend = T, legend = "bottom")

ggsave("preliminary_figs/meeting_jan_2024/RP_rank_FG_facet.png", width = 5.5, height = 4)
