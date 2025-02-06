
filtered_RR = edge_RR2 %>%
  filter(!(species %in% c(Kdrop$species) & site == "KNZ"),
         !(species %in% c(Hdrop$species) & site == "HYS"),
         !(species %in% c(Cdrop$species) & site == "CHY"),
         !(species %in% c(SGdrop$species) & site == "SGS"),
         !(species %in% c(SLdrop$species) & site == "SBL"),
         !(species %in% c(SKdrop$species) & site == "SBK"))


filtered_RR %>%
  filter(Duration != "unknown") %>%
ggplot(aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Duration)

ggplot(filtered_RR, aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site)

