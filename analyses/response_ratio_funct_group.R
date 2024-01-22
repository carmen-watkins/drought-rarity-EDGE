
source("analyses/calculate_response_ratio.R") 
source("data-prep/funct_group_key.R")
theme_set(theme_classic())

# Join FG Data ####
edge_temp <- edge_w_predictors.site %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG <- left_join(edge_temp, genuskey, by = "genus")


edge_temp2 <- edge_w_predictors.site.recov %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG_recov <- left_join(edge_temp2, genuskey, by = "genus")


# Overall FG Patterns ####
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

# Site Level Patterns ####
ggplot(edge_FG, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~site)

ggplot(edge_FG_recov, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Recovery Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~site)

ggplot(edge_FG, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~site)

ggplot(edge_FG_recov, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
  facet_wrap(~site)

# Facet by FG
RD <- ggplot(edge_FG, aes(x=percrank, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  #geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~FunctionalGroup, ncol = 5)

RR <- ggplot(edge_FG_recov, aes(x=percrank, y=resp.ratio.site)) +
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

ggsave("preliminary_figs/resp_ratio_rank_persistence/Fig4_RR_rank_FG_facet.png", width = 8, height = 4)


PD <- ggplot(edge_FG, aes(x=persistence.site, y=resp.ratio.site)) +
  geom_point(aes(color = FunctionalGroup), alpha = 0.9, size = 0.9) +
  #geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  geom_smooth(method = "lm", alpha = 0, color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  #guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  facet_wrap(~FunctionalGroup, ncol = 5)

PR <- ggplot(edge_FG_recov, aes(x=persistence.site, y=resp.ratio.site)) +
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

ggsave("preliminary_figs/resp_ratio_rank_persistence/Fig5_RP_rank_FG_facet.png", width = 8, height = 4)


