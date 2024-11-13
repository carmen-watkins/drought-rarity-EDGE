# Header #### 
## Script name: Classify Rank, Persistence with Zero filled data
##
## Purpose of script: Classify each species at each site by its rank and persistence at the site using data from control plots in the EDGE experiment.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up env ####
## read in cleaned cover data
source("data-prep/cleaning_fill_zeros_at_subplot_edge.R") 

library(ggpubr)

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
rank_persist <- left_join(persist_site, rank_mean, by = c("site", "species"))

# Clean Env ####
rm(controls, persist_site, rank_mean)
