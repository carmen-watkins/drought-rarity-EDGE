

lost_recov_sp <- edge_FG %>%
  filter(quad %in% c("lost", "recovered"))




persist <- ggplot(lost_recov_sp, aes(x=persistence.site)) +
  geom_histogram() +
  facet_wrap(~quad) +
  xlab("Persistence")

rank <- ggplot(lost_recov_sp, aes(x=percrank)) +
  geom_histogram() +
  facet_wrap(~quad) +
  xlab("Rank")

ggarrange(rank, persist, ncol = 1, nrow = 2)

ggsave("preliminary_figs/meeting_jan_2024/lost_recov_R_P.png",  height = 4.5, width = 5)
