source("data-prep/clean_edge_data.R")
theme_set(theme_classic())


## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

ggplot(edge_all, aes(x=year, y=total.plot.cover, color = treatment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site)

edge_meancov <- edge_all %>%
  group_by(site, year, treatment) %>%
  summarise(mean.cov = mean(total.plot.cover), 
            se.cov = calcSE(total.plot.cover))

edge_meancov$site <- as.factor(edge_meancov$site)
edge_meancov <- edge_meancov %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black"))

ggplot(edge_meancov, aes(x=year, y=mean.cov, color = treatment)) +
  geom_point() +
  geom_line() +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  scale_color_manual(values = c("#008080", "#ca562c")) +
  ylab("Average Total Cover") +
  xlab("Year") +
  labs(color = "Treatment") +
  geom_errorbar(aes(ymin = mean.cov - se.cov, ymax = mean.cov + se.cov), width = 0.25)

ggsave("preliminary_figs/meeting_jan_2024/total_cov_timeseries.png", width = 5, height = 4.5)

#008080,#70a494,#b4c8a8,#f6edbd,#edbb8a,#de8a5a,#ca562c


