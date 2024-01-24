source("data-prep/clean_edge_data.R")
FG <- read.csv("data/edge_species_info.csv")

theme_set(theme_classic())


sp.list <- edge_all %>%
  group_by(site) %>%
  select(site, species, spcode, kartez) %>%
  distinct()


sp.list.FG <- left_join(sp.list, FG, by = c("species"))

na.check <- sp.list.FG %>%
  filter(is.na(FunctionalGroup))

sum.stats <- sp.list.FG %>%
  group_by(site, FunctionalGroup) %>%
  summarise(SR.FG = n())

sum.stats$site <- as.factor(sum.stats$site)

sum.stats <- sum.stats %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))


ggplot(sum.stats, aes(x=FunctionalGroup, y=SR.FG, fill = FunctionalGroup)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  ylab("Functional Group Richness") + 
  xlab("Functional Group") +
  scale_fill_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606"))
  
ggsave("preliminary_figures/FG_richness_site.png", height = 5, width = 5.5)

