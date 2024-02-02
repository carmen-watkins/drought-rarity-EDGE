# Investigating which species are having interesting responses---------

source("analyses/calculate_response_ratio.R")
theme_set(theme_classic())

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


distrib_sp <- c("Bouteloua_gracilis", "Lepidium_densiflorum", "Gutierrezia_sarothrae", "Machaeranthera_pinnatifida", "Plantago_patagonica")

indivsp <- edge_FG %>%
  filter(species %in% distrib_sp)

indivsp$species <- factor(indivsp$species, levels = c("Bouteloua_gracilis", "Lepidium_densiflorum", "Gutierrezia_sarothrae", "Machaeranthera_pinnatifida", "Plantago_patagonica"))

ggplot(indivsp, aes(x=drought.RR, recovery.RR)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(
    #data=subset(edge_FG,species=="Bouteloua_gracilis"), 
             aes(fill=percrank), color = "black", pch = 21, size=3) +
  facet_wrap(~species, ncol = 5) +
  scale_fill_viridis_c(direction = -1)

ggsave("preliminary_figs/meeting_jan_2024/indiv_sp_together_rank.png", width = 10, height = 2.5)

ggplot(indivsp, aes(x=drought.RR, recovery.RR)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(
    #data=subset(edge_FG,species=="Bouteloua_gracilis"), 
    aes(fill=persistence.site), color = "black", pch = 21, size=3) +
  facet_wrap(~species, ncol = 5) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "persistence")

ggsave("preliminary_figs/meeting_jan_2024/indiv_sp_together_persistence.png", width = 10, height = 2.5)







## Visualize common species
bougra <- ggplot(edge_FG, aes(x=drought.RR, recovery.RR)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  #facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  #scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  geom_point(data=subset(edge_FG,species=="Bouteloua_gracilis"), 
             aes(x=drought.RR, y=recovery.RR, fill=percrank), color = "black", pch = 21, size=4) +
  scale_fill_viridis_c(direction = -1) +
  ggtitle("Bouteloua gracilis")

#ggsave("preliminary_figs/meeting_jan_2024/bougra_strat_rank.png", width = 6, height = 3)

lepden <- ggplot(edge_FG, aes(x=drought.RR, recovery.RR)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  #facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  #scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  geom_point(data=subset(edge_FG,species=="Lepidium_densiflorum"), 
             aes(x=drought.RR, y=recovery.RR, fill=as.factor(percrank)), color = "black", pch = 21, size=4) +
  scale_fill_manual(values = c("#fcde9c","#faa476","#f0746e", "#e34f6f", "#b9257a","#7c1d6f")) +
  ggtitle("Lepidium Densiflorum")

#ggsave("preliminary_figs/meeting_jan_2024/lepnit_strat_rank.png", width = 6, height = 3)

gutsar <- ggplot(edge_FG, aes(x=drought.RR, recovery.RR)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  #facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  #scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  geom_point(data=subset(edge_FG,species=="Gutierrezia_sarothrae"), 
             aes(x=drought.RR, y=recovery.RR, fill=as.factor(percrank)), color = "black", pch = 21, size=4) +
  scale_fill_manual(values = c("#fcde9c","#faa476","#f0746e", "#e34f6f", "#b9257a","#7c1d6f"))


macpin <- ggplot(edge_FG, aes(x=drought.RR, recovery.RR)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  #facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  #scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  geom_point(data=subset(edge_FG,species=="Machaeranthera_pinnatifida"), 
             aes(x=drought.RR, y=recovery.RR, fill=as.factor(percrank)), color = "black", pch = 21, size=4) +
  scale_fill_manual(values = c("#fcde9c","#f0746e", "#e34f6f", "#b9257a","#7c1d6f")) +
  ggtitle("Machaeranthera pinnatifida")

plapat <- ggplot(edge_FG, aes(x=drought.RR, recovery.RR)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  #facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  #scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606")) +
  geom_point(data=subset(edge_FG,species=="Plantago_patagonica"), 
             aes(x=drought.RR, y=recovery.RR, fill=as.factor(percrank)), color = "black", pch = 21, size=4) +
  scale_fill_manual(values = c("#fcde9c","#f0746e", "#e34f6f", "#b9257a","#7c1d6f")) +
  ggtitle("Plantago patagonica")

ggarrange(bougra, lepden, gutsar, macpin, plapat, nrow = 2, ncol = 3)






