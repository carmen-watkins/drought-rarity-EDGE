# Investigating which species are having interesting responses---------

## Make a table of species occurrences to see which species occur across sites
spp_table <- edge_FG %>%
  select(species) %>%
  group_by(species) %>%
  summarize(n=n())%>%
  arrange(desc(n))
#Bouteloua gracilis (blue grama) and Lepidium densiflorum (pepperweed) occur all 6 sites

spp_sum <- spp_table %>%
  group_by(n) %>%
  summarise(n_sites_sp = n())

ggplot(spp_sum, aes(x=n, y=n_sites_sp)) +
  geom_point(size = 3) +
  ylab("Number of Species") +
  xlab("Number of Sites")

ggsave("preliminary_figs/meeting_jan_2024/sites_per_sp.png", width = 4, height = 3)

## Visualize common species
ggplot(edge_FG, aes(x=drought.RR, recovery.RR, color = FunctionalGroup)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  geom_point(data=subset(edge_FG,species=="Bouteloua_gracilis"), 
             aes(x=drought.RR, y=recovery.RR, shape=as.factor(percrank)), size=4, color = "black") +
  scale_shape_manual(values = c(20, 20, 16, 19, 19))

ggsave("preliminary_figs/meeting_jan_2024/bougra_strat_rank.png", width = 6, height = 5)

ggplot(edge_FG, aes(x=drought.RR, recovery.RR, color = FunctionalGroup)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  geom_point(data=subset(edge_FG,species=="Lepidium_densiflorum"), 
             aes(x=drought.RR, y=recovery.RR, shape=as.factor(percrank)), size=4, color = "black") +
  scale_shape_manual(values = c(20, 18, 19, 19, 15, 15))

ggsave("preliminary_figs/meeting_jan_2024/lepden_strat_rank.png", width = 6, height = 5)




