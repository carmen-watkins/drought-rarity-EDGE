## This script generates the rank abundance
## curves for each site
## By: Beatriz A. Aguirre

#load required libraries
library(dplyr)
library(ggplot2)

#import data raw data pre-classification
source("data-prep/clean_edge_data.R")

#Subset control data
control.data <- edge_all %>% 
  filter(treatment == "C")

#calculate rank of the mean
rank_mean_by_year <- control.data %>%
  group_by(site, species) %>% ## take the mean of a species at a site right away
  summarise(mean.cov = mean(max.cover)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(rank2 = rank(mean.cov, na.last = NA, ties.method = "last"), perc_rank2 = percent_rank(mean.cov))
# NOTE: rank and perc_rank are on different scales
# Low rank means that cover is low; high rank means that cover is hgh
# 1.00 perc_rank means the highest cover and most abundant species

#Generate rank abundance curve: 
x <-rank_mean_by_year %>% 
  mutate(rank2 = rank((rank2))) %>% 
  ggplot(aes(rank2, mean.cov)) + 
  geom_bar(stat="identity", position=position_dodge(), color="black") +
  ylim(0,NA) +
  scale_x_reverse() +
  xlab("Rank") +
  ylab("Mean Cover")

x + facet_grid(~site)

#####################################
## Calculate total species 
## richness by site: 
#####################################
spp_richness <- control.data %>%
  group_by(site) %>% 
  summarise(spp_richness = n_distinct(species), 
            .groups = 'drop')
spp_richness

#####################################
## View sites individually on its own: 
#####################################
s1 <-control_rank %>% 
  filter(site=="CHY") %>% 
  mutate(rank = rank(desc(rank))) %>% 
  ggplot(aes(perc_rank, max.cover)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,NA) +
  scale_x_reverse() +
  xlab("Percent Rank") +
  ylab("Mean Cover") +
  annotate("text", x=.5, y=80, label= 'italic("CHY-- species richness = 74")',
           col="black", size=5, parse=TRUE)
s1
