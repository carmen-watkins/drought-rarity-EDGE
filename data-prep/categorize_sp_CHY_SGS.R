
source("data-prep/clean_edge_data.R")

unique(edge_all$site)

## need to classify species at CHY and SGS still as we don't have long term data from these yet.

chy_sgs <- edge_all %>%
  filter(site == "CHY" | site == "SGS") %>%
  filter(treatment == "C", max.cover > 0) ## filter to keep only controls 

### Rank Sp by Abundance ####
datrank_cs = chy_sgs %>%
  group_by(site, block, plot, subplot) %>%
  mutate(rank = rank(max.cover, na.last = NA, ties.method = "average"), percrank = percent_rank(max.cover)) 
## this is the rank for each species in each subplot
## currently rank and percrank are opposing. 1 is the lowest rank, while 1.00 percrank means the highest biomass.
## greater rank percentile means that it is more abundant.


### Calc Mean Rank & Yrs Present ####
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

### Create Categories ####
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
