# Header #### 
## Script name: Classify Rank, Persistence
##
## Purpose of script: Classify each species at each site by its rank and persistence at the site using data from control plots in the EDGE experiment.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up env ####
## read in cleaned cover data
source("data-prep/clean_edge_data.R") 

library(ggpubr)

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## filter data to include control plots only
## use edge data with zeros for accurate calculations
controls <- edge_w_zeros %>%
  filter(treatment == "C")


# New Calcs ####
## Rank ####
## take the rank of the mean (NOT the mean of the rank)
## keep the 0's
rank_mean <- controls %>%
  group_by(site, species) %>% ## take the mean of a species at a site right away
  summarise(mean.ctrl.cov = mean(mean.plot.cover)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(percrank = percent_rank(mean.ctrl.cov)) ## take the percent rank

#rank_mean2 <- edge_all %>%
 # filter(treatment == "C") %>%
  #group_by(site, species) %>% ## take the mean of a species at a site right away
  #summarise(mean.cov = mean(mean.plot.cover)) %>%
  #ungroup() %>%
  #group_by(site) %>%
  #mutate(percrank = percent_rank(mean.cov))

## compare visually
#withzeros <- ggplot(rank_mean, aes(x=-percrank, y=mean.cov)) +
 # geom_point() +
  #facet_wrap(~site) +
  #ggtitle("With Zeros")
#nozeros <- ggplot(rank_mean2, aes(x=-percrank, y=mean.cov)) +
 # geom_point() +
  #facet_wrap(~site) +
  #ggtitle("No Zeros")

#ggarrange(withzeros, nozeros)
#ggsave("preliminary_figs/compare_RAC_w_zeros.png", width = 8, height = 4)

## Persistence ####
### plot ####
## decided not to use plot level persistence to keep scale consistent b/w rank & persistence
persist_plot <- controls %>%
  group_by(site, block, plot, species) %>% ## do I need to group by year at all first? no
  summarise(persistence.plot = sum(pres.abs)/n())
## across years calculate the number of years present and divide by the total number of years
## do this at each site for every block, plot, and species

### block ####
## there is only one control plot per block so this is now the same thing as calculating at the plot level
#persist_block <- controls %>%
 # group_by(site, block, species, year) %>%
#  summarise(pres.abs.block = ifelse(sum(pres.abs)>0, 1, 0)) %>% ## present in block?
 # ungroup() %>%
  #group_by(site, block, species) %>%
  #summarise(persistence.block = sum(pres.abs.block)/n())

### site ####
persist_site <- controls %>%
  group_by(site, species, year) %>%
  summarise(pres.abs.site = ifelse(sum(pres.abs)>0, 1,0)) %>% ## present at site?
  ungroup() %>%
  group_by(site, species) %>%
  summarise(persistence.site = sum(pres.abs.site)/n())

### merge ####
persist_all <- left_join(persist_plot, persist_site, by = c("site", "species")) %>%
  pivot_longer(persistence.plot:persistence.site, names_to = "spatial.scale", values_to = "persistence") %>%
  group_by(site, species, spatial.scale) %>%
  summarise(mean.persist = mean(persistence)) %>%
  pivot_wider(names_from = "spatial.scale", values_from = "mean.persist")

persist_sum <-  left_join(persist_plot, persist_site, by = c("site", "species")) %>%
  pivot_longer(persistence.plot:persistence.site, names_to = "spatial.scale", values_to = "persistence") %>%
  group_by(site, species, spatial.scale) %>%
  summarise(mean.persist = mean(persistence)) %>%
  ungroup() %>%
  group_by(site, spatial.scale) %>%
  summarise(mean.persist.overall = mean(mean.persist), se.persist = calcSE(mean.persist))

## change site to factor to visualize
persist_sum$site <- as.factor(persist_sum$site)
persist_sum <- persist_sum %>%
  mutate(site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

### explore spatial scale ####
ggplot(persist_sum, aes(x=site, y=mean.persist.overall, color = spatial.scale)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean.persist.overall - se.persist, ymax = mean.persist.overall+se.persist), width = 0.25) +
  ylab("Mean Persistence") + xlab("")
#ggsave("preliminary_figs/persistence_spatial_scale.png", width = 5, height = 3)

# Merge Rank & Persist ####
rank_persist <- left_join(persist_all, rank_mean, by = c("site", "species"))

# Clean Env ####
rm(controls, persist_all, persist_plot, persist_site, persist_sum, rank_mean)

# Old Notes ####
## calculate the mean cover of a species across years first before ranking
## now we know how we're handling year vs. leaving it up to the rank function (we don't know if this would affect transient species when they're not present)
## objective is to classify species at a site

## persistence - what spatial scale to measure it at?
## maybe

## presence or absence across the site - in any given year, what proportion of sites have a species (spatial rarity) - divide 
## perisstence - drop any plot where it never showed up
## % of years that it showed up
## perennial vs annual might be a good approximation
## how many times does a species show up in a plot in any given year


## correlate %persistence and %rank
## rank abundance- how much do species shuffle
## look at Megan Avolio's rank shift work - do subordinate species shuffle up in ranks more in semi-arid
## codyn additions
