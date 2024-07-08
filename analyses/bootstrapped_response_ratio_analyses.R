# Analyse bootstrapped values

# Set up ####
source()
source()

# Drought RR ####
## Calc non-bootstrapped comparison ####
resp.ratio.site <- edge_all %>%
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>% ## make columns of cover in D and C treatments
  replace(is.na(.), 0) %>% ## input 0 instead of NAs (NAs would be present where there is no cover of a particular species in either drought or control)
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D)) ## calc response ratio

## Merge rank & persistence ####
## merge with rank and persistence values for each species
## non-bootstrapped
edge_w_predictors.site <- left_join(resp.ratio.site, rank_persist, by = c("site", "species"))
## bootstrapped
edge_w_predictors.site.bootstrap <- left_join(resp.ratio.site_bootstrapped_sum, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site$site <- as.factor(edge_w_predictors.site$site)
edge_w_predictors.site <- edge_w_predictors.site %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")))
dim(edge_w_predictors.site)

edge_w_predictors.site.bootstrap$site <- as.factor(edge_w_predictors.site.bootstrap$site)
edge_w_predictors.site.bootstrap <- edge_w_predictors.site.bootstrap %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SBL", "SBK")))
dim(edge_w_predictors.site.bootstrap)
#413   10

# Recovery ####
## Calc non-bootstrapped comparison ####
resp.ratio.site.recov <- edge_all %>%
  filter(treatment.year == "recovery") %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>% ## make columns of cover in D and C treatments
  replace(is.na(.), 0) %>% ## input 0 instead of NAs (NAs would be present where there is no cover of a particular species in either drought or control)
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D))

## Merge rank & persistence ####
## merge with rank and persistence values for each species
edge_w_predictors.site.recov <- left_join(resp.ratio.site.recov, rank_persist, by = c("site", "species"))
edge_w_predictors.site.recov.bootstrap <- left_join(resp.ratio.site.recov_bootstrapped_sum, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site.recov$site <- as.factor(edge_w_predictors.site.recov$site)
edge_w_predictors.site.recov <- edge_w_predictors.site.recov %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

edge_w_predictors.site.recov.bootstrap$site <- as.factor(edge_w_predictors.site.recov.bootstrap$site)
edge_w_predictors.site.recov.bootstrap <- edge_w_predictors.site.recov.bootstrap %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

# Sanity check ####
#Let's see how well the bootstrapped results match the original calculations 
edge_w_predictors.site.bootstrap_TEMP<-
  inner_join(edge_w_predictors.site,edge_w_predictors.site.bootstrap)

edge_w_predictors.site.recov.bootstrap_TEMP<-
  inner_join(edge_w_predictors.site.recov,edge_w_predictors.site.recov.bootstrap)


#Graph of the regression
d <- ggplot(edge_w_predictors.site.bootstrap_TEMP,
       aes(x=resp.ratio.site,
           y=resp.ratio.site.mean))+
  geom_point()+
  geom_errorbar(aes(ymin=resp.ratio.site.mean-resp.ratio.site.margin,
                    ymax=resp.ratio.site.mean+resp.ratio.site.margin))+
  annotate("label", x=-0.5,y=1, 
           label=paste("Pearson r =",signif(cor(edge_w_predictors.site.bootstrap_TEMP$resp.ratio.site,
                                                edge_w_predictors.site.bootstrap_TEMP$resp.ratio.site.mean,
                                                method = "pearson"),3)))+ #Including the correlation in the plot
  theme_classic() +
  xlab("Drought Response Ratio") +
  ylab("Mean Drought Response Ratio")

r <- ggplot(edge_w_predictors.site.recov.bootstrap_TEMP,
       aes(x=resp.ratio.site,
           y=resp.ratio.site.mean))+
  geom_point()+
  geom_errorbar(aes(ymin=resp.ratio.site.mean-resp.ratio.site.margin,
                    ymax=resp.ratio.site.mean+resp.ratio.site.margin))+
  annotate("label", x=-0.5,y=1, 
           label=paste("Pearson r =",signif(cor(edge_w_predictors.site.recov.bootstrap_TEMP$resp.ratio.site,
                                                edge_w_predictors.site.recov.bootstrap_TEMP$resp.ratio.site.mean,
                                                method = "pearson"),3)))+ #Including the correlation in the plot
  theme_classic() +
  xlab("Recovery Response Ratio") +
  ylab("Mean Recovery Response Ratio")

ggarrange(d, r)

ggsave("preliminary_figs/june_2024/bootstrap_RRR_sanity_check.png", width = 6, height = 3)


# Merge DR & RR Data ####
drought.RR.bootstrap <- edge_w_predictors.site.bootstrap %>%
  mutate(treatment.period = "drought.RR") %>%
  select(site, treatment.period, species, resp.ratio.site.mean, 
         resp.ratio.site.margin, persistence.site, percrank)

recov.RR.bootstrap <- edge_w_predictors.site.recov.bootstrap %>%
  mutate(treatment.period = "recovery.RR") %>%
  select(site, treatment.period, species, resp.ratio.site.mean, 
         resp.ratio.site.margin,persistence.site, percrank)

## merge drought & recov dataframes
response.ratio.tog.bootstrap <- rbind(drought.RR.bootstrap, recov.RR.bootstrap) %>%
  mutate(precip.bin = ifelse(site %in% c("KNZ", "HYS"), "high",
                             ifelse(site %in% c("CHY", "SGS"), "med", "low"))) %>%
  pivot_wider(names_from = treatment.period, values_from = c(resp.ratio.site.mean,resp.ratio.site.margin)) %>%
  mutate(resp.ratio.site.mean_drought.RR = ifelse(is.na(resp.ratio.site.mean_drought.RR), 0, resp.ratio.site.mean_drought.RR),
         resp.ratio.site.mean_recovery.RR = ifelse(is.na(resp.ratio.site.mean_recovery.RR), 0, resp.ratio.site.mean_recovery.RR))

## recreate figure 4
ggplot(response.ratio.tog.bootstrap, aes(x=resp.ratio.site.mean_drought.RR, y=resp.ratio.site.mean_recovery.RR, color = percrank)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.25) +
  geom_point()+
  facet_wrap(~site, nrow = 1, ncol = 6) +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_gradientn(
    colors = c("#E3B710", "#DCCB4E", "#BDC881", "#A2A475", "#81A88D", "#00A08A", "#0B775E", "#175149")) +
  labs(color = "Rank") +
  geom_errorbar(aes(ymin=resp.ratio.site.mean_recovery.RR-resp.ratio.site.margin_recovery.RR,
                    ymax=resp.ratio.site.mean_recovery.RR+resp.ratio.site.margin_recovery.RR)) +
  geom_errorbarh(aes(xmin = resp.ratio.site.mean_drought.RR - resp.ratio.site.margin_drought.RR,
                     xmax = resp.ratio.site.mean_drought.RR + resp.ratio.site.margin_drought.RR))
  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  coord_cartesian(ylim = c(-1.25, 1.25
  )) +
  annotate(geom="text", x=-0.75, y=1.2, label="-D+R",
           color="black", size = 3) +
  annotate(geom="text", x=-0.75, y=-1.2, label="-D-R",
           color="black", size = 3) +
  annotate(geom="text", x=0.75, y=1.2, label="+D+R",
           color="black", size = 3) +
  annotate(geom="text", x=0.75, y=-1.2, label="+D-R",
           color="black", size = 3)


