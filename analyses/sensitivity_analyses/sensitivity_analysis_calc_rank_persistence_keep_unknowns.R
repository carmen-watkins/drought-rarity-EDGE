
source("analyses/sensitivity_analyses/clean_cover_dat_fill_zeros_KEEP_UNKNOWNS_sensA.R")

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## filter data to include control plots only
## use edge data with zeros for accurate calculations
controls <- edge_all %>%
  filter(treatment == "C")

# Rank ####
## take the rank of the mean (NOT the mean of the rank)
## keep the 0's
rank_mean <- controls %>%
  group_by(site, species) %>% ## take the mean of a species at a site right away
  ## this averages over all the subplots, including 0-filled subs
  summarise(mean.ctrl.cov = mean(max.cover)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(percrank = percent_rank(mean.ctrl.cov), ## take the percent rank
         absrank = rank(mean.ctrl.cov)) 

# Persistence ####
persist_site <- controls %>%
  group_by(site, species, year) %>%
  summarise(pres.abs.site = ifelse(sum(pres.abs)>0, 1,0)) %>% ## present at site?
  ungroup() %>%
  group_by(site, species) %>%
  summarise(persistence.site = sum(pres.abs.site)/n())

# Merge Rank & Persist ####
rank_persist <- left_join(persist_site, rank_mean, by = c("site", "species")) %>%
  mutate(spatial_rarity = 1 - percrank,
         temporal_rarity = 1 - persistence.site)

# Clean Env ####
rm(controls, persist_site, rank_mean, test, test2, testsub, testws2, weirdsubs, weirdsubs2)
