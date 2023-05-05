## This script works on determining
## dominance of a species by 
## calculating frequency of a species 
## at each site across plots 
## By: Beatriz A. Aguirre

#import data raw data pre-classification
source("data-prep/clean_edge_data.R") #note imported data is already subset to only include controls

#remove irrelevant data frames
rm(chy, hay, knz, sev_black, sev_blue, sgs, edge_all)


##########Harmonize subplot column names across sites

#check how many levels of subplot there are by site
table(edge_w_zeros$site, edge_w_zeros$subplot)

#harmonize subplots
edge_data_zeros <- edge_w_zeros %>%
  mutate(subplot = ifelse(
    subplot == "1", "A",
    ifelse(subplot == "2", "B",
           ifelse(subplot == "3", "C",
                  ifelse(subplot == "4", "D", subplot)
           )
    )
  ))

#check that subplot levels are renamed for SEV sites
table(edge_data_zeros$site, edge_data_zeros$subplot)

#remove old df
rm(edge_w_zeros)

###############################################################

#calculate frequency (sum of pres.abs column) by site 
#for each species each year by plot
freq_df <- edge_data_zeros %>%
  group_by(site, year, treatment, block, plot, species) %>% 
  summarize(spp_pres_plot = sum(pres.abs)/n()) %>% #percent present (1.0 indicates always present)
  ungroup() %>%
  group_by(site, year, treatment, block, plot, species)

#Need to think about if it's worth averaging percent present 
#across blocks by species & what this tells us relative to time


##Visualize frequency of species over time by site
figure1 <-block_freq_df %>% 
  filter(site=="KNZ") %>% 
  ggplot(aes(year, prec_pres_block, color=species)) + 
  geom_point() +
  ylim(0,NA) +
  xlab("Year") +
  ylab("species frequency %")

figure1 + facet_grid(~species)




