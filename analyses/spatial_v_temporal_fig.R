

edge_RR_preds$site = factor(edge_RR_preds$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

ggplot(edge_RR_preds, aes(x=spatial_rarity, y=temporal_rarity)) +
  geom_hline(yintercept = 0.5, color = "lightgrey") +
  geom_vline(xintercept = 0.5, color = "lightgrey") +
  geom_point() +
  facet_wrap(~site, nrow=1, ncol=6) +
  ylab("Temporal Rarity") +
  xlab("Spatial Rarity")

ggsave("figures/Dec2024/st_rarity.png", width = 9.5, height = 2.25)



