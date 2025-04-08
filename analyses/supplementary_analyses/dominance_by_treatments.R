

source("data-prep/classify_rank_persistence.R")

# Calc Berger-Parker Dominance ####
## the relative abundance of the most abundant species in the plot
BP_dominance = edge_all %>%
  group_by(year, treatment, site, block, plot, species) %>%
  summarise(mean.cov.plot = mean(max.cover)) %>%
  group_by(year, treatment, site, plot) %>%
  summarise(tot.cov = sum(mean.cov.plot), 
            dom.cov = max(mean.cov.plot),
            BP.dom = dom.cov/tot.cov) %>%
  mutate(site = as.factor(site),
         site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

a1 = aov(BP.dom ~ site * treatment, data = BP_dominance)
summary(a1)

TukeyHSD(a1)

BP_dominance %>%
  group_by(site, treatment) %>%
  summarise(BP.dom.site = mean(BP.dom), 
            se.dom.site = calcSE(BP.dom)) %>%
  
  ggplot(aes(x=treatment, y=BP.dom.site)) +
 # geom_bar(stat = "identity") +
  geom_point() +
  geom_errorbar(aes(ymin = BP.dom.site - se.dom.site, ymax = BP.dom.site + se.dom.site), width = 0.25) +
  facet_wrap(~site, ncol = 6) +
  ylab("Berger-Parker Dominance") +
  xlab("Drought Treatment")
  

ggsave("figures/Mar2025/FigS_dom_bw_treatments.tiff", width = 8, height = 3.5)



