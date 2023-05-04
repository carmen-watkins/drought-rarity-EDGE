## categorize species by LT control data OR by the EDGE control data only
source("data-prep/clean_edge_data.R")

library(ggpubr)

# Final Data Cleaning ####
## Fill 0's ####
sev_black <- edge_all %>%
  filter(site == "SEV_black", treatment == "C") %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_eriopoda:Chamaesyce_albomarginata, names_to = "species", values_to = "max.cover")
  
sev_blue <- edge_all %>%
  filter(site == "SEV_blue", treatment == "C") %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Bouteloua_gracilis:Sporobolus_contractus, names_to = "species", values_to = "max.cover")

hay <- edge_all %>%
  filter(site == "HYS", treatment == "C") %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Achillea_millefolium:Croton_sp., names_to = "species", values_to = "max.cover")

knz <- edge_all %>%
  filter(site == "KNZ", treatment == "C") %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Ambrosia_psilostachya:Sonchus_asper, names_to = "species", values_to = "max.cover")

chy <- edge_all %>%
  filter(site == "CHY", treatment == "C") %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Allium_textile:Sporobolus_sp., names_to = "species", values_to = "max.cover")

sgs <- edge_all %>%
  filter(site == "SGS", treatment == "C") %>%
  select(-spcode, -kartez) %>%
  pivot_wider(names_from = "species", values_from = "max.cover", values_fill = 0) %>%
  pivot_longer(Aristida_purpurea:ASOX, names_to = "species", values_to = "max.cover")

edge_w_zeros <- do.call("rbind", list(sev_black, sev_blue, hay, knz, chy, sgs))

# New Calcs ####
## Rank ####
## take the rank of the mean (NOT the mean of the rank)
## keep the 0's
rank_mean_by_year <- edge_w_zeros %>%
  group_by(site, species) %>% ## take the mean of a species at a site right away
  summarise(mean.cov = mean(max.cover)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(rank = rank(mean.cov, na.last = NA, ties.method = "average"), percrank = percent_rank(mean.cov)) 

rank_mean_by_year2 <- edge_all %>%
  group_by(site, species) %>% ## take the mean of a species at a site right away
  summarise(mean.cov = mean(max.cover)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(rank = rank(mean.cov, na.last = NA, ties.method = "average"), percrank = percent_rank(mean.cov))


withzeros <- ggplot(rank_mean_by_year, aes(x=-percrank, y=mean.cov)) +
  geom_point() +
  facet_wrap(~site) +
  ggtitle("With Zeros")
nozeros <- ggplot(rank_mean_by_year2, aes(x=-percrank, y=mean.cov)) +
  geom_point() +
  facet_wrap(~site) +
  ggtitle("No Zeros")


ggarrange(withzeros, nozeros)


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



# Old Rank Calcs ####
## Rank Sp by Abundance ####
datrank_cs <- class %>%
  group_by(site, block, plot, subplot, year) %>%
  mutate(rank = rank(max.cover, na.last = NA, ties.method = "average"), percrank = percent_rank(max.cover)) 
## this is the rank for each species in each subplot in each year
## currently rank and percrank are opposing. 1 is the lowest rank, while 1.00 percrank means the highest biomass.
## greater rank percentile means that it is more abundant.
## percent rank is important for comparing sites because of differences in richness.



## Calc Mean Rank & Yrs Present ####
# mean rank percentile, sum number of years present, for each species in each plot.
categorydat_cs <- datrank_cs %>%
  group_by(site, block, plot, subplot, species) %>%
  summarize(mean_rank = mean(percrank), # mean rank% cover for all years (does not include zeros)
            yrs_present = n(), # number of years species was present = number of rows for Taxon
            # (no zeros in working_coverdat dataset)
            yrs_present_list = paste0("yr", as.character(year), collapse = ", "),
            .groups = "keep")

#ggplot(categorydat, aes(x=yrs_present)) +
#geom_histogram()


# how many years per plot?
nyearsdat_cs <- datrank_cs %>%
  group_by(site, block, plot, subplot) %>%
  summarize(n_years = length(unique(year)), .groups = "keep")

summary(nyearsdat_cs)
# 10 years

## Create Categories ####
## Retrying classification at the field level.
field_catdat_cs <- categorydat_cs %>%
  merge(y = nyearsdat_cs) %>%
  mutate(persist_pct = yrs_present / n_years) %>%
  group_by(site, species) %>%
  summarise(mrank = mean(mean_rank), mpersist_pct = mean(persist_pct)) %>%
  mutate(core_cat = case_when(mpersist_pct < 0.5 ~ "transient",
                              mpersist_pct >= 0.5 ~ "core"), # if present half of the time or more, core
         core_cat = factor(core_cat, levels = c("transient", "core")),
         dom_cat = case_when(mrank < 0.5 ~ "subordinate",
                             mrank >= 0.5 ~ "dominant"), # if, on average, more dominant than half of the spp in the plots, dominant
         dom_cat = factor(dom_cat, levels = c("subordinate", "dominant"))) %>%
  unite(combo, core_cat, dom_cat, remove = FALSE, sep = "_") %>%
  merge(y = data.frame(combo = c("transient_subordinate", "transient_dominant", "core_subordinate", "core_dominant"),
                       nickname = c("TransSub", "TransDom", "CoreSub", "CoreDom")) ) %>%
  mutate(nickname = factor(nickname, levels = c("CoreDom", "CoreSub", "TransDom", "TransSub")))

rm(categorydat_cs, chy_sgs, datrank_cs, nyearsdat_cs)

