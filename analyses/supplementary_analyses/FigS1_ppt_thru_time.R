
# Set up ####
## load packages
library(tidyverse)
library(ggpubr)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## read in precip data
precip <- read.csv("data/growingseason_precip_totals_allyears.csv")
sev_ppt <- read.csv("data/sev_download/sev298_NPP_edge_biomass.csv")

source("analyses/color_palettes.R")

## set up graphics
theme_set(theme_classic())
pal <- wes_palette("Royal3")

# Clean data ####
sev_temporal = sev_ppt %>%
  select(site, year, season, season.precip) %>%
  distinct() %>%
  mutate(site = ifelse(site == "EDGE_black", "SBK", "SBL")) %>%
  filter(season == "fall") %>%
  select(-season) 

names(sev_temporal) = c("site", "year", "tot.precip")

north_temporal <- precip %>%
  group_by(Site, Year) %>%
  summarise(tot.precip = sum(ambient_precip))
  
names(north_temporal) = c("site", "year", "tot.precip")

ppt_all = rbind(sev_temporal, north_temporal) %>%
  mutate(site = as.factor(site), 
         site = fct_relevel(site, "KNZ", "HYS", "CHY", "SGS", "SBL", "SBK"))

# Plot ####
ggplot(ppt_all, aes(x = year, y = tot.precip, color = site)) +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Precipitation (mm)") +
  labs(color = "Site") +
  scale_color_manual(values = pal) 

ggsave("figures/Mar2025/fig_s1_precip_year.tiff", width = 6, height = 4)  

