library(tidyverse)

syn_dat <- read.csv("Synthesis_Master_2019_Grasslands.csv")

unique(syn_dat$site)
## overlapping sites: 
  ## hay, knz, sev
sort(unique(syn_dat[syn_dat$site == "hay",]$year))

syndat_filtered <- syn_dat %>%
  filter(site %in% c("hay", "knz", "sev"))

length(unique(syndat_filtered[syndat_filtered$site == "hay",]$year)) ## 30 years
length(unique(syndat_filtered[syndat_filtered$site == "knz",]$year)) ## 33 years
length(unique(syndat_filtered[syndat_filtered$site == "sev",]$year)) ## 16 years
## should consider filtering out uniqueIDs with fewer than a certain number of years. It looks like some are as low as 2 years and this could skew the persist_pct calc

ggplot(syndat_filtered, aes(x=abundance)) +
  geom_histogram()

ggplot(syndat_filtered[syndat_filtered$abundance == 0,], aes(x=abundance)) +
  geom_histogram()
## no abundance = 0

### Rank Sp by Abundance ####
datrank = syndat_filtered %>%
  group_by(site, plot, subplot, uniqueID) %>%
  mutate(rank = rank(abundance, na.last = NA, ties.method = "average"), percrank = percent_rank(abundance)) 
## this is the rank for each species in each subplot
## currently rank and percrank are opposing. 1 is the lowest rank, while 1.00 percrank means the highest biomass.
## greater rank percentile means that it is more abundant.

# QUESTION ####
  ## should this be done grouped by each year or NOT? - at this point no- since we're using mutate it ranks every year.

### Calc Mean Rank & Yrs Present ####
# mean rank percentile, sum number of years present, for each species in each plot.
categorydat <- datrank %>%
  group_by(site, plot, subplot, uniqueID, species) %>%
  summarize(mean_rank = mean(percrank), # mean rank% cover for all years (does not include zeros)
            yrs_present = length(percrank), # number of years species was present = number of rows for Taxon
            # (no zeros in working_coverdat dataset)
            yrs_present_list = paste0("yr", as.character(year), collapse = ", "),
            .groups = "keep")

ggplot(categorydat, aes(x=yrs_present)) +
  geom_histogram()


# how many years per plot?
nyearsdat <- datrank %>%
  group_by(site, plot, subplot, uniqueID) %>%
  summarize(n_years = length(unique(year)), .groups = "keep")

summary(nyearsdat)
# 2-33 years

### Create Categories ####
## Retrying classification at the field level.
field_catdat <- categorydat %>%
  merge(y = nyearsdat) %>%
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

