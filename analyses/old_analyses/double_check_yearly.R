

source("data-prep/classify_rank_persistence.R")
source("data-prep/clean_edge_data.R")

FG <- read.csv("data/edge_species_info.csv")

## Resp Ratio Yearly ####
resp.ratio.yearly <- edge_all %>%
  ungroup() %>%
  select(-spcode, -kartez, -plot, -block, -year) %>% ## remove extraneous cols that will mess up pivoting
  group_by(site, treatment, species, experiment.year, treatment.year) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% 
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  group_by(site, species, experiment.year, treatment.year) %>%
  mutate(resp.ratio.site = (D-C)/(C+D))

## merge with rank & persistence vals
edge_yearly <- left_join(resp.ratio.yearly, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_yearly$site <- as.factor(edge_yearly$site)
edge_yearly <- edge_yearly %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

## create a key of years
## will be used to match up spei data to particular treatment years
year.key <- edge_all %>%
  group_by(site, experiment.year, treatment.year, year) %>%
  summarise(year2 = unique(year)) 

edge_yearly2 <- left_join(edge_yearly, year.key, by = c("site", "experiment.year", "treatment.year"))

edge_yearly_FG <- left_join(edge_yearly2, FG, by = "species") %>%
  filter(FunctionalGroup != "tree", !is.na(FunctionalGroup)) 

edge_yearly_FG$site <- as.factor(edge_yearly_FG$site)
edge_yearly_FG <- edge_yearly_FG %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

## Rank ####
ggplot(edge_yearly_FG, aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Rank") +
  ylab("Response Ratio") +
  labs(color= "Exp Year")


ggplot(edge_yearly_FG, aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Persistence") +
  ylab("Response Ratio") +
  labs(color= "Exp Year")



## Rank ####
ggplot(edge_yearly_FG[edge_yearly_FG$treatment.year == "drought",], aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Rank") +
  ylab("Drought Response Ratio") +
  labs(color = "Drought Year")

## Persistence ####
ggplot(edge_yearly_FG[edge_yearly_FG$treatment.year == "drought",], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 2, nrow = 3) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#b4c8a8", "#edbb8a","#de8a5a", "#ca562c")) +
  xlab("Persistence") +
  ylab("Drought Response Ratio") +
  labs(color = "Drought Year")


# Recov years only ####
## Rank ####
ggplot(edge_yearly_FG[edge_yearly_FG$treatment.year == "recovery",], aes(x=percrank, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#de8a5a", "#edbb8a","#b4c8a8","#70a494","#008080" )) +
  xlab("Rank") +
  ylab("Recovery Response Ratio") +
  labs(color = "Recovery Year")

#ggsave("preliminary_figs/resp_ratio_rank_persistence/rank_RR_yearly_recov_site_faceted.png", width = 10, height = 6)

## Persistence ####
ggplot(edge_yearly_FG[edge_yearly_FG$treatment.year == "recovery",], aes(x=persistence.site, y=resp.ratio.site, color = as.factor(experiment.year))) +
  geom_point() +
  facet_wrap(~site, ncol = 3, nrow = 2) +
  geom_smooth(method = "lm", alpha = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#de8a5a", "#edbb8a","#b4c8a8","#70a494","#008080" )) +
  xlab("Persistence") +
  ylab("Recovery Response Ratio") +
  labs(color = "Recovery Year")












