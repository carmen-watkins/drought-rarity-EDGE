
source("analyses/calculate_response_ratio.R") 
FG <- read.csv("data/edge_species_info.csv")

theme_set(theme_classic())

# Join FG Data ####
edge_temp <- edge_w_predictors.site %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG <- left_join(edge_temp, FG, by = "species")

edge_temp2 <- edge_w_predictors.site.recov %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG_recov <- left_join(edge_temp2, FG, by = "species")

# Fig 2: sp strategies ####
## Figure ####
drought.RR <- edge_FG %>%
  mutate(resp.ratio.drought = resp.ratio.site,
         mean.cov.drought = mean.cov) %>%
  select(site, species, resp.ratio.drought, mean.cov.drought, persistence.site, percrank, FunctionalGroup)

recov.RR <- edge_FG_recov %>%
  mutate(resp.ratio.recov = resp.ratio.site,
         mean.cov.recov = mean.cov) %>%
  select(site, species, resp.ratio.recov, mean.cov.recov, FunctionalGroup)

## merge drought & recov dataframes
response.ratio.tog <- left_join(drought.RR, recov.RR, by = c("site", "species", "FunctionalGroup")) %>%
  mutate(precip.bin = ifelse(site %in% c("KNZ", "HYS"), "high",
                             ifelse(site %in% c("CHY", "SGS"), "med", "low")))

response.ratio.tog$precip.bin <- as.factor(response.ratio.tog$precip.bin)

response.ratio.tog <- response.ratio.tog %>%
  mutate(precip.bin = fct_relevel(precip.bin, "high", "med", "low"))

## visualize
ggplot(response.ratio.tog, aes(x=resp.ratio.drought, resp.ratio.recov, color = FunctionalGroup)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_smooth(method = "lm", alpha = 0.10, color = "black", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606"))

ggsave("preliminary_figs/resp_ratio_rank_persistence/Fig2_DRR_v_RRR_FG.png", width = 5, height = 4.5)

## Precip Version ####
ggplot(response.ratio.tog, aes(x=resp.ratio.drought, resp.ratio.recov, color = precip.bin)) +
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
  
ggsave("preliminary_figs/resp_ratio_rank_persistence/DRR_v_RRR_precip_bin.png", width = 7, height = 2.5)


## Summary Stats ####
sumRR <- response.ratio.tog %>% 
  mutate(quad = ifelse(resp.ratio.drought > 0 & resp.ratio.recov > 0, "Q2",
                          ifelse(resp.ratio.drought > 0 & resp.ratio.recov < 0, "Q4",
                                 ifelse(resp.ratio.drought < 0 & resp.ratio.recov < 0, "Q3",
                                        ifelse(resp.ratio.drought < 0 & resp.ratio.recov > 0,"Q1", NA))))) %>%
  group_by(site, quad) %>%
  summarise(quad.tot = n()) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(overall.tot= sum(quad.tot),
         percent.quad = quad.tot/overall.tot)

sumRR.ppt <- response.ratio.tog %>% 
  mutate(quad = ifelse(resp.ratio.drought > 0 & resp.ratio.recov > 0, "Q2",
                       ifelse(resp.ratio.drought > 0 & resp.ratio.recov < 0, "Q4",
                              ifelse(resp.ratio.drought < 0 & resp.ratio.recov < 0, "Q3",
                                     ifelse(resp.ratio.drought < 0 & resp.ratio.recov > 0,"Q1", NA))))) %>%
  group_by(precip.bin, quad) %>%
  summarise(quad.tot = n()) %>%
  ungroup() %>%
  group_by(precip.bin) %>%
  mutate(overall.tot= sum(quad.tot),
         percent.quad = quad.tot/overall.tot)


ggplot(sumRR, aes(x=quad, y=percent.quad)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  xlab("Quadrant") + ylab("% Species of Total")

ggsave("preliminary_figs/sp_percent_quadrants_site.png", height = 5, width = 5)

ggplot(sumRR.ppt, aes(x=quad, y=percent.quad)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~precip.bin, ncol = 3, nrow = 1) +
  xlab("Quadrant") + ylab("% Species of Total")

ggsave("preliminary_figs/sp_percent_quadrants_ppt.png", height = 2, width = 5)

## Precip Facet x FG color ####
ggplot(response.ratio.tog, aes(x=resp.ratio.drought, resp.ratio.recov, color = FunctionalGroup)) +
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

ggsave("preliminary_figs/resp_ratio_rank_persistence/Fig2_DRR_v_RRR_facetppt_colorFG.png", width = 7, height = 2.5)

# Figure 3: Overall FG patterns ####
FGrankD <- ggplot(edge_FG, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

FGrankR <- ggplot(edge_FG_recov, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Recovery Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

FGpersD <- ggplot(edge_FG, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

FGpersR <- ggplot(edge_FG_recov, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio") +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

ggarrange(FGrankD, FGpersD, FGrankR, FGpersR, labels = "AUTO", common.legend = T, legend = "bottom")

ggsave("preliminary_figs/resp_ratio_rank_persistence/Fig3_RR_drought_recov_FG.png", width = 6.5, height = 6.5)

# Other Analyses ####
## Site Level Patterns ####
ggplot(edge_FG, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~site)

ggsave("preliminary_figs/resp_ratio_rank_persistence/DR_FG_by_site.png", width = 7, height = 3.5)

ggplot(edge_FG_recov, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Recovery Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~site)

ggsave("preliminary_figs/resp_ratio_rank_persistence/RR_FG_by_site.png", width = 7, height = 3.5)

ggplot(edge_FG, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~site)

ggsave("preliminary_figs/resp_ratio_rank_persistence/DP_FG_by_site.png", width = 7, height = 3.5)

ggplot(edge_FG_recov, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~site)

ggsave("preliminary_figs/resp_ratio_rank_persistence/RP_FG_by_site.png", width = 7, height = 3.5)


## Facet by FG #####
RD <- ggplot(edge_FG[edge_FG$FunctionalGroup != "tree" & !is.na(edge_FG$FunctionalGroup),], aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  #geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~FunctionalGroup, ncol = 5)

RR <- ggplot(edge_FG_recov[edge_FG_recov$FunctionalGroup != "tree" & !is.na(edge_FG_recov$FunctionalGroup),], aes(x=percrank, y=resp.ratio.site)) +
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

ggsave("preliminary_figs/resp_ratio_rank_persistence/Fig4_RR_rank_FG_facet.png", width = 5.5, height = 4)

PD <- ggplot(edge_FG[edge_FG$FunctionalGroup != "tree" & !is.na(edge_FG$FunctionalGroup),], aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  #geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~FunctionalGroup, ncol = 5)

PR <- ggplot(edge_FG_recov[edge_FG_recov$FunctionalGroup != "tree" & !is.na(edge_FG_recov$FunctionalGroup),], aes(x=persistence.site, y=resp.ratio.site)) +
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

ggsave("preliminary_figs/resp_ratio_rank_persistence/Fig5_RP_rank_FG_facet.png", width = 5.5, height = 4)






