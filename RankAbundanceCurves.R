## This script generates the rank abundance
## curves for each site
## By: Beatriz A. Aguirre

#load required libraries
library(dplyr)
library(ggplot2)

#import data raw data pre-classification
source("data-prep/clean_edge_data.R")

#Subset control data
control.data <- edge_w_zeros %>% 
  filter(treatment == "C")

#calculate rank of the mean
rank_mean_by_year <- control.data %>%
  group_by(site, species) %>% ## take the mean of a species at a site right away
  summarise(mean.cov = mean(max.cover)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(rank2 = rank(mean.cov, na.last = NA, ties.method = "last"), perc_rank2 = percent_rank(mean.cov))
# NOTE: rank and perc_rank are on different scales
# Low rank means that cover is low; high rank means that cover is high
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
s1 <-rank_mean_by_year %>% 
  filter(site=="CHY") %>% 
  mutate(rank2 = rank(rank2)) %>% 
  ggplot(aes(rank2, mean.cov)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,NA) +
  scale_x_reverse() +
  xlab("Rank") +
  ylab("Mean Cover") +
  annotate("text", x=60, y=30, label= 'italic("CHY-- species richness = 74")',
           col="black", size=5, parse=TRUE)
s1
#####################################
s2 <-rank_mean_by_year %>% 
  filter(site=="HYS") %>% 
  mutate(rank2 = rank(rank2)) %>% 
  ggplot(aes(rank2, mean.cov)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,NA) +
  scale_x_reverse() +
  xlab("Rank") +
  ylab("Mean Cover") +
  annotate("text", x=60, y=28, label= 'italic("HYS-- species richness = 89")',
           col="black", size=5, parse=TRUE)
s2
#####################################
s3 <-rank_mean_by_year %>% 
  filter(site=="KNZ") %>% 
  mutate(rank2 = rank(rank2)) %>% 
  ggplot(aes(rank2, mean.cov)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,NA) +
  scale_x_reverse() +
  xlab("Rank") +
  ylab("Mean Cover") +
  annotate("text", x=30, y=50, label= 'italic("KNZ-- species richness = 63")',
           col="black", size=5, parse=TRUE)
s3
#####################################
s4 <-rank_mean_by_year %>% 
  filter(site=="SEV_black") %>% 
  mutate(rank2 = rank(rank2)) %>% 
  ggplot(aes(rank2, mean.cov)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,NA) +
  scale_x_reverse() +
  xlab("Rank") +
  ylab("Mean Cover") +
  annotate("text", x=20, y=30, label= 'italic("SEV_black-- species richness = 54")',
           col="black", size=5, parse=TRUE)
s4
#####################################
s5 <-rank_mean_by_year %>% 
  filter(site=="SEV_blue") %>% 
  mutate(rank2 = rank(rank2)) %>% 
  ggplot(aes(rank2, mean.cov)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,NA) +
  scale_x_reverse() +
  xlab("Rank") +
  ylab("Mean Cover") +
  annotate("text", x=40, y=20, label= 'italic("SEV_blue-- species richness = 63")',
           col="black", size=5, parse=TRUE)
s5
#####################################
s6 <-rank_mean_by_year %>% 
  filter(site=="SGS") %>% 
  mutate(rank2 = rank(rank2)) %>% 
  ggplot(aes(rank2, mean.cov)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,NA) +
  scale_x_reverse() +
  xlab("Rank") +
  ylab("Mean Cover") +
  annotate("text", x=40, y=25, label= 'italic("SGS-- species richness = 57")',
           col="black", size=5, parse=TRUE)
s6
#####################################
