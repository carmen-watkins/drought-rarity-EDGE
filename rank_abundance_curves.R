
## make rank abundance curves
rank_mean <- edge_all %>%
  group_by(site, species, treatment) %>% ## take the mean of a species at a site right away
  summarise(mean.cov = mean(mean.plot.cover)) %>%
  ungroup() %>%
  group_by(site, treatment) %>%
  mutate(percrank = percent_rank(mean.cov)) 


rank_mean$site = factor(rank_mean$site, levels = c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

ggplot(rank_mean, aes(x=percrank, y=mean.cov)) +
  geom_point() +
  facet_grid(treatment~site) +
  scale_x_reverse() +
  theme_bw() +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0.75, linetype = "dashed", color = "red") +
  xlab("Percent Rank") +
  ylab("Mean Species Cover")

ggsave("figures/final_figs/supp/figure_s4.png", width = 10, height = 4)


persist_site <- edge_w_zeros %>%
  group_by(site, species, year, treatment) %>%
  summarise(pres.abs.site = ifelse(sum(pres.abs)>0, 1,0),
            mean.year.cov = mean(mean.plot.cover)) %>% ## present at site?
  ungroup() %>%
  group_by(site, species, treatment) %>%
  summarise(persistence.site = sum(pres.abs.site)/n(), 
            mean.cov = mean(mean.year.cov)) 

ggplot(persist_site, aes(x=persistence.site, y=mean.cov)) +
  geom_point() +
  facet_grid(treatment~site) +
  scale_x_reverse() +
  theme_bw() +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0.75, linetype = "dashed", color = "red") +
  xlab("Persistence") +
  ylab("Mean Species Cover")

median(persist_site$persistence.site)

