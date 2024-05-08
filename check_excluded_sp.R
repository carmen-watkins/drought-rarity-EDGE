

sgs <- edge_all %>%
  filter(site == "SGS", experiment.year %in% c(1:4))

sort(unique(sgs$species))

lowrep.sgs <- lowrep.sp %>%
  filter(site == "SGS")

low.sp <- unique(lowrep.sgs$species)

sgslow <- sgs %>%
  filter(species %in% low.sp)

ggplot(sgslow, aes(x=year, y=mean.plot.cover, color = treatment)) +
  geom_point() +
  facet_wrap(~species)
