
# Set up ####
## load packages
library(tidyverse)
library(ggpubr)
library(cowplot)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## read in precip data
precip = read.csv("data/growingseason_precip_totals_allyears.csv")
sev_ppt = read.csv("data/sev_download/sev298_NPP_edge_biomass.csv")

source("data-prep/clean_spei_data.R")
source("analyses/color_palettes.R")

## set up graphics
theme_set(theme_classic())
pal = c("#03274E", "#3B5378", "#7F5F70",
        "#CE685E", "#E5AA7F", "#FCD484")

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
ppt_time = ggplot(ppt_all, aes(x = year, y = tot.precip)) +
  
  geom_rect(aes(xmin = 2013, xmax = 2019, ymin = -Inf, 
                ymax = Inf), fill="#E6E6E6", alpha = .2) +
  
  geom_point(aes(color = site), size = 3) +
  geom_line(aes(color = site)) +
  xlab("Year") +
  ylab("Precipitation (mm)") +
  labs(color = "Site") +
  scale_color_manual(values = pal) +
  geom_vline(xintercept = 2014, color = "black", linetype = "dashed") +
  geom_vline(xintercept = 2017, color = "black", linetype = "dashed") +
  theme(text = element_text(size = 14))

  
## explore distrib of SPEI over all years
spei_time = spei_all %>%
  ungroup() %>%
  ggplot(aes(x=spei)) +
  geom_density() +
  facet_wrap(~site, ncol = 5, nrow = 1) +
  geom_jitter(data = spei_exp, aes(x=spei, y=0.1, color = exp.year, fill = exp.year),
              height = 0.01, width = 0, alpha = 0.5, size = 2) +
  scale_color_manual(values = c( "#D69C4E", "#435163", "white")) +
  scale_fill_manual(values = c( "#D69C4E", "#435163")) +
  xlab("Growing Seasion SPEI") +
  ylab("Frequency") +
  labs(color = "Experiment Year", fill = "Experiment Year") +
  guides(fill = "none") +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 14))

plot_grid(ppt_time, spei_time, ncol = 1, labels = c("(a)", "(b)"))

ggsave("figures/review_figs/precip_year_FigSX.tiff", width = 8, height = 6)  

