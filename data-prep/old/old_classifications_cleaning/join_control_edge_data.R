# Set up Env ####
library(tidyverse)

## Read in Data ####
## species categorizations (synchrony data)
source("data-prep/clean_long_term_control_data.R")

## EDGE data
source("data-prep/clean_edge_data.R")

## category data CHY & SGS
source("data-prep/categorize_sp_CHY_SGS.R")

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

theme_set(theme_bw())

# Final Cleaning ####
ggplot(field_catdat, aes(x=nickname)) +
  geom_bar() +
  facet_wrap(~site)

ggplot(field_catdat, aes(x=mpersist_pct)) +
  geom_histogram()
## much better

## Join sp classifications with EDGE data ####

## done separately for each site as the species codes are different for each site.

### SEV ####
sev_edge <- edge_all %>%
  filter(site == "SEV_black" | site == "SEV_blue") %>%
  mutate(species.name = species) %>%
  select(-species) %>%
  mutate(species = kartez)

sev_edge_class <- left_join(sev_edge, field_catdat[field_catdat$site == "sev",], by = c("species")) %>%
  select(-species) %>%
  mutate(species = species.name) %>%
  select(-species.name) %>%
  mutate(site = site.x) %>%
  select(-site.y, -site.x)

tmp <- sev_edge_class %>%
  filter(nickname == "TransDom", site == "SEV_black")
unique(tmp$species)
  
### KNZ ####
knz_edge <- edge_all %>%
  filter(site == "KNZ") %>%
  mutate(site = "knz")

field_catdat_knz <- field_catdat %>%
  filter(site == "knz") %>%
  mutate(genuscode = toupper(substr(species, 1, 3)), 
         sp.ep = strsplit(species, "_") %>%
           sapply(tail, 1),
         spepcode = toupper(substr(sp.ep, 1, 3)),
         spcode = paste0(genuscode, spepcode)) %>%
  select(-genuscode, -sp.ep, -spepcode, -species)

knz_edge_class <- left_join(knz_edge, field_catdat_knz, by = c("spcode", "site"))# %>%
  #mutate(field = NA)

field_catdat_hay <- field_catdat %>%
  filter(site == "hay") %>%
  mutate(genuscode = toupper(substr(species, 1, 3)), 
         sp.ep = strsplit(species, " ") %>%
           sapply(tail, 1),
         spepcode = toupper(substr(sp.ep, 1, 3)),
         spcode = paste0(genuscode, spepcode)) %>%
  select(-genuscode, -sp.ep, -spepcode, -species)


hay_edge <- edge_all %>%
  filter(site == "HYS") %>%
  mutate(site = "hay")

hay_edge_class <- left_join(hay_edge, field_catdat_hay, by = c("spcode", "site")) #%>%
  #mutate(field = NA)

td <- hay_edge_class %>%
  filter(nickname == "TransDom")
unique(td$species)
# CHY & SGS ####
chy_sgs_class <- left_join(edge_all[edge_all$site == "SGS" | edge_all$site == "CHY", ], field_catdat_cs, by = c("site", "species"))



## join together
temp <- rbind(sev_edge_class, knz_edge_class)
edge3 <- rbind(temp, hay_edge_class)

edge_all_class <- rbind(edge3, chy_sgs_class)

rm(categorydat, chy_sgs_class, datrank, edge_all, edge3, field_catdat, field_catdat_cs, field_catdat_hay, field_catdat_knz, hay_edge, hay_edge_class, knz_edge, knz_edge_class, nyearsdat, sev_edge, sev_edge_class, syn_dat, syndat_filtered, temp)
