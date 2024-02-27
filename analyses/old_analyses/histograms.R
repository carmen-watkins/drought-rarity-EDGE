

ggplot(edge_FG, aes(x=drought.RR)) +
  geom_histogram() +
  facet_wrap(~site) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed")

ggsave("preliminary_figs/meeting_jan_2024/drought_hist.png", width = 6, height = 4)

ggplot(edge_FG, aes(x=recovery.RR)) +
  geom_histogram() +
  facet_wrap(~site) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed")

ggsave("preliminary_figs/meeting_jan_2024/recov_hist.png", width = 6, height = 4)
