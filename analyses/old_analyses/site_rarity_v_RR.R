
ps = edge_RR %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  ggplot(aes(color = site)) +
  scale_color_manual(values = pal) +
  
  geom_point(aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_smooth(method = "lm", aes(x=spatial_rarity, y=resp.ratio.site_D4), alpha = 0.15) +
  geom_hline(yintercept = 0) +
  facet_wrap(~site, ncol = 6) +
  geom_point(aes(x=spatial_rarity, y=resp.ratio.site_PDfull), pch = 1) +
  geom_smooth(method = "lm", aes(x=spatial_rarity, y=resp.ratio.site_PDfull), linetype = "dashed", alpha = 0.15) +
  xlab("Spatial Rarity") +
  ylab("Response Ratio") +
  coord_cartesian(ylim = c(-1,1))

pt = edge_RR %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")) %>%
  ggplot(aes(color = site)) +
  scale_color_manual(values = pal) +
  
  geom_point(aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_smooth(method = "lm", aes(x=temporal_rarity, y=resp.ratio.site_D4), alpha = 0.25) +
  geom_hline(yintercept = 0) +
  facet_wrap(~site, ncol = 6) +
  geom_point(aes(x=spatial_rarity, y=resp.ratio.site_PDfull), pch = 1) +
  geom_smooth(method = "lm", aes(x=temporal_rarity, y=resp.ratio.site_PDfull), linetype = "dashed", alpha = 0.25) +
  xlab("Temporal Rarity") +
  ylab("Response Ratio") +
  coord_cartesian(ylim = c(-1,1))


ggarrange(ps, pt, ncol =1, common.legend = T)







p1 = ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_D4, color = site)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0)

p2 = ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0)

p3 = ggplot(edge_RR, aes(x=spatial_rarity, y=resp.ratio.site_PDfull)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0)

p4 = ggplot(edge_RR, aes(x=temporal_rarity, y=resp.ratio.site_PDfull)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0)

ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
