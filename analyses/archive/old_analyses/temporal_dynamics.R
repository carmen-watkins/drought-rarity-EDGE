# Header #### 
## Script name: Temporal Dynamics
##
## Purpose of script: Explore the temporal dynamics of species cover
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

source("data-prep/clean_edge_data.R")


colnames(edge_all)


colnames(rank_persist)


dom.sp <- rank_persist %>%
  group_by(site) %>%
 # filter(percrank > 0.9) %>%
  mutate(rank.cat = ifelse(percrank > 0.9, "dominant", "subordinate"))

edge_all_dom <- left_join(edge_all, dom.sp[,c(1,2,7)], by = c("site", "species"))


knz <- edge_all_dom %>%
  filter(site == "KNZ") %>%
  group_by(experiment.year, treatment.year, year, treatment, species, spcode, rank.cat) %>%
  summarise(mean.cov.year = mean(mean.plot.cover))

ggplot(knz, aes(x=experiment.year, y=mean.cov.year, color = species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~treatment*rank.cat)

sev_blue <- edge_all_dom %>%
  filter(site == "SEV_blue") %>%
  group_by(experiment.year, treatment.year, year, treatment, species, spcode, rank.cat) %>%
  summarise(mean.cov.year = mean(mean.plot.cover))

ggplot(sev_blue, aes(x=experiment.year, y=mean.cov.year, color = species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~treatment*rank.cat)

sev_black <- edge_all_dom %>%
  filter(site == "SEV_black") %>%
  group_by(experiment.year, treatment.year, year, treatment, species, spcode, rank.cat) %>%
  summarise(mean.cov.year = mean(mean.plot.cover))

ggplot(sev_black, aes(x=experiment.year, y=mean.cov.year, color = species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~treatment*rank.cat)

chy <- edge_all_dom %>%
  filter(site == "CHY") %>%
  group_by(experiment.year, treatment.year, year, treatment, species, spcode, rank.cat) %>%
  summarise(mean.cov.year = mean(mean.plot.cover))

ggplot(chy, aes(x=experiment.year, y=mean.cov.year, color = species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~treatment*rank.cat)

sgs <- edge_all_dom %>%
  filter(site == "SGS") %>%
  group_by(experiment.year, treatment.year, year, treatment, species, spcode, rank.cat) %>%
  summarise(mean.cov.year = mean(mean.plot.cover))

ggplot(sgs, aes(x=experiment.year, y=mean.cov.year, color = species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~treatment*rank.cat)

hys <- edge_all_dom %>%
  filter(site == "HYS") %>%
  group_by(experiment.year, treatment.year, year, treatment, species, spcode, rank.cat) %>%
  summarise(mean.cov.year = mean(mean.plot.cover))

ggplot(hys, aes(x=experiment.year, y=mean.cov.year, color = species)) +
  geom_point() +
  geom_line() +
  facet_wrap(~treatment*rank.cat)

unique(edge_all_dom$site)




