# Header #### 
## Script name: Calculate Response Ratio
##
## Purpose of script: Calculate the response ratio between drought and control plots 1. across years during two time periods (drought & recovery) and 2. for each year. Add in functional group data as well.
##
## Author: Carmen Watkins
##
## Email: cebel2@uoregon.edu

# Set up env ####
## read in cleaned data
source("data-prep/classify_rank_persistence.R")
source("data-prep/clean_edge_data.R")

FG <- read.csv("data/edge_species_info.csv")
"%w/o%" <- function(x,y)!('%in%'(x,y))
# Resp Ratio Across Years ####
## During Drought ####
## (drought - control)/control + drought

### Block Level ##
## NOT using block level anymore
#resp.ratio.block <- edge_all %>%
 # filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  #group_by(site, block, treatment, species) %>%
#  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by block across years
 # pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  #ungroup() %>%
#  group_by(site, block, species) %>%
 # mutate(resp.ratio.block = (D-C)/(C+D)) 

## merge with rank and persistence values for each species
#edge_w_predictors.block <- left_join(resp.ratio.block, rank_persist, by = c("site", "species"))

## change site to an ordered factor
#edge_w_predictors.block$site <- as.factor(edge_w_predictors.block$site)
#edge_w_predictors.block <- edge_w_predictors.block %>%
 # mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

### Site Level ####


low.rep.resp.ratio.site <- edge_all %>%
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  summarise(total.samps=n())%>%
  filter(total.samps>=4) %>%
  ungroup()%>%
  distinct(site,species)%>%
  left_join(edge_all,
            join_by(site,species))%>%
  group_by(site, treatment, species) %>%
  slice_sample(prop = (1-0.2))%>%
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D)) 




random_spp_mat_drought <- function(orig_mat, frac_samp=NULL, min_samples=NULL){
  resp.ratio.site.temp <- orig_mat %>%
    filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
    group_by(site, treatment, species) %>%
    summarise(total.samps=n())%>%
    filter(total.samps>=min_samples) %>%
    ungroup()%>%
    distinct(site,species)%>%
    left_join(orig_mat,
              join_by(site,species))%>%
    slice_sample(prop = (1-frac_samp))%>%
    group_by(site, treatment, species) %>%
    summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
    pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    group_by(site, species) %>%
    mutate(resp.ratio.site = (D-C)/(C+D)) 
  
  
  return(resp.ratio.site.temp)
  

}

  


resp.ratio.site.bootstrap.df <- function(orig_dibble,
                                         frac_samp=NULL, 
                                         min_samples=NULL,
                                         boots_perm=1){
  
  random_site_ratio_temp<-data.frame("int_num"= as.numeric(),
                                "site" = as.character(),
                                "species" = as.character(),
                                "C" = as.numeric(),
                                "D" = as.numeric(), 
                                "resp.ratio.site" = as.numeric())
  for (i in 1:boots_perm) {
    random.site.mat.drought<-suppressMessages(random_spp_mat_drought(orig_dibble,
                                                 frac_samp=frac_samp, 
                                                 min_samples=min_samples))
    
    random_site_ratio_temp<-rbind(random_site_ratio_temp,
                             data.frame("int_num"= rep_len(i,nrow(random.site.mat.drought)),
                                        "site" = random.site.mat.drought$site,
                                        "species" = random.site.mat.drought$species,
                                        "C" = random.site.mat.drought$C,
                                        "D" = random.site.mat.drought$D, 
                                        "resp.ratio.site" = random.site.mat.drought$resp.ratio.site))
  }
  temp_random_spp_mat_drought<-random_spp_mat_drought(orig_dibble,
                                frac_samp=frac_samp, 
                                min_samples=min_samples)
  random_site_ratio_temp2<-
    orig_dibble%>%
    filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
    filter(paste(site,species)%w/o% paste(temp_random_spp_mat_drought$site,
                                          temp_random_spp_mat_drought$species))%>%
    group_by(site, treatment, species) %>%
    summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
    pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    group_by(site, species) %>%
    mutate(resp.ratio.site = (D-C)/(C+D),
           int_num=rep(NA)) %>%
    bind_rows(random_site_ratio_temp)
    
    
  return(tibble(random_site_ratio_temp2))
}

temp_test_dir<-resp.ratio.site.bootstrap.df(edge_all, 
                             frac_samp=0.2, 
                             min_samples=4,
                             boots_perm=10)


temp_test_dir_sum<- 
  temp_test_dir%>%
  group_by(site, species)%>%
  summarise(resp.ratio.site.mean=mean(resp.ratio.site),
            resp.ratio.site.median=median(resp.ratio.site),
            resp.ratio.site.margin=qt(0.975,df=n()-1)*sd(resp.ratio.site)/sqrt(n()))


ggplot(temp_test_dir_sum,
       aes(x=species,y=resp.ratio.site.mean))+
  geom_point()+
  geom_errorbar(aes(ymin=resp.ratio.site.mean-resp.ratio.site.margin,
                    ymax=resp.ratio.site.mean+resp.ratio.site.margin))

#Original ratio Code####
resp.ratio.site <- edge_all %>%
  filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D)) 

## merge with rank and persistence values for each species
resp.ratio.site_bootstrapped<-resp.ratio.site.bootstrap.df(edge_all, 
                                            frac_samp=0.2, 
                                            min_samples=4,
                                            boots_perm=100)


resp.ratio.site_bootstrapped_sum<- 
  resp.ratio.site_bootstrapped%>%
  group_by(site, species)%>%
  summarise(resp.ratio.site.mean=mean(resp.ratio.site),
            resp.ratio.site.median=median(resp.ratio.site),
            resp.ratio.site.margin=qt(0.975,df=n()-1)*sd(resp.ratio.site)/sqrt(n()))



edge_w_predictors.site.bootstrap <- left_join(resp.ratio.site_bootstrapped_sum, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site.bootstrap$site <- as.factor(edge_w_predictors.site.bootstrap$site)
edge_w_predictors.site.bootstrap <- edge_w_predictors.site.bootstrap %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

## During Recovery ####
### Site Level ####

random_spp_mat_recov <- function(orig_mat, frac_samp=NULL, min_samples=NULL){
  resp.ratio.site.temp <- orig_mat %>%
    filter(treatment.year == "recovery") %>% 
    group_by(site, treatment, species) %>%
    summarise(total.samps=n())%>%
    filter(total.samps>=min_samples) %>%
    ungroup()%>%
    distinct(site,species)%>%
    left_join(orig_mat,
              join_by(site,species))%>%
    slice_sample(prop = (1-frac_samp))%>%
    group_by(site, treatment, species) %>%
    summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
    pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    group_by(site, species) %>%
    mutate(resp.ratio.site = (D-C)/(C+D)) 
  
  
  return(resp.ratio.site.temp)
  
  
}




resp.ratio.site.recov.bootstrap.df <- function(orig_dibble,
                                         frac_samp=NULL, 
                                         min_samples=NULL,
                                         boots_perm=1){
  
  random_site_ratio_temp<-data.frame("int_num"= as.numeric(),
                                     "site" = as.character(),
                                     "species" = as.character(),
                                     "C" = as.numeric(),
                                     "D" = as.numeric(), 
                                     "resp.ratio.site" = as.numeric())
  for (i in 1:boots_perm) {
    random.site.mat.recov<-suppressMessages(random_spp_mat_recov(orig_dibble,
                                                                     frac_samp=frac_samp, 
                                                                     min_samples=min_samples))
    
    random_site_ratio_temp<-rbind(random_site_ratio_temp,
                                  data.frame("int_num"= rep_len(i,nrow(random.site.mat.recov)),
                                             "site" = random.site.mat.recov$site,
                                             "species" = random.site.mat.recov$species,
                                             "C" = random.site.mat.recov$C,
                                             "D" = random.site.mat.recov$D, 
                                             "resp.ratio.site" = random.site.mat.recov$resp.ratio.site))
  }
  temp_random_spp_mat_recov<-random_spp_mat_recov(orig_dibble,
                                                      frac_samp=frac_samp, 
                                                      min_samples=min_samples)
  random_site_ratio_temp2<-
    orig_dibble%>%
    filter(treatment.year == "recovery") %>% 
    filter(paste(site,species)%w/o% paste(temp_random_spp_mat_recov$site,
                                          temp_random_spp_mat_recov$species))%>%
    group_by(site, treatment, species) %>%
    summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
    pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    group_by(site, species) %>%
    mutate(resp.ratio.site = (D-C)/(C+D),
           int_num=rep(NA)) %>%
    bind_rows(random_site_ratio_temp)
  
  
  return(tibble(random_site_ratio_temp2))
}

temp_test_dir<-resp.ratio.site.recov.bootstrap.df(edge_all, 
                                            frac_samp=0.2, 
                                            min_samples=4,
                                            boots_perm=10)


temp_test_dir_sum<- 
  temp_test_dir%>%
  group_by(site, species)%>%
  summarise(resp.ratio.site.mean=mean(resp.ratio.site),
            resp.ratio.site.median=median(resp.ratio.site),
            resp.ratio.site.margin=qt(0.975,df=n()-1)*sd(resp.ratio.site)/sqrt(n()))


ggplot(temp_test_dir_sum,
       aes(x=species,y=resp.ratio.site.mean))+
  geom_point()+
  geom_errorbar(aes(ymin=resp.ratio.site.mean-resp.ratio.site.margin,
                    ymax=resp.ratio.site.mean+resp.ratio.site.margin))

#Original recovery ratio####
resp.ratio.site.recov <- edge_all %>%
  filter(treatment.year == "recovery") %>% 
  group_by(site, treatment, species) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  group_by(site, species) %>%
  mutate(resp.ratio.site = (D-C)/(C+D))

## merge with rank and persistence values for each species

resp.ratio.site.recov_bootstrapped<-resp.ratio.site.recov.bootstrap.df(edge_all, 
                                                           frac_samp=0.2, 
                                                           min_samples=4,
                                                           boots_perm=100)

resp.ratio.site.recov_bootstrapped_sum<- 
  resp.ratio.site.recov_bootstrapped%>%
  group_by(site, species)%>%
  summarise(resp.ratio.site.mean=mean(resp.ratio.site),
            resp.ratio.site.median=median(resp.ratio.site),
            resp.ratio.site.margin=qt(0.975,df=n()-1)*sd(resp.ratio.site)/sqrt(n()))


edge_w_predictors.site.recov.bootstrap <- left_join(resp.ratio.site.recov_bootstrapped_sum, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site.recov.bootstrap$site <- as.factor(edge_w_predictors.site.recov.bootstrap$site)
edge_w_predictors.site.recov.bootstrap <- edge_w_predictors.site.recov.bootstrap %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

## Resp Ratio Yearly ####
resp.ratio.yearly <- edge_all %>%
  ungroup() %>%
  select(-spcode, -kartez, -plot, -block, -year) %>% ## remove extraneous cols that will mess up pivoting
  group_by(site, treatment, species, experiment.year, treatment.year) %>%
  summarise(mean.cover.sp = mean(mean.plot.cover)) %>% 
  pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
  ungroup() %>%
  group_by(site, species, experiment.year, treatment.year) %>%
  mutate(resp.ratio.site = (D-C)/(C+D))

## merge with rank & persistence vals
edge_yearly_w_predictors <- left_join(resp.ratio.yearly, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_yearly_w_predictors$site <- as.factor(edge_yearly_w_predictors$site)
edge_yearly_w_predictors <- edge_yearly_w_predictors %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))

## create a key of years
## will be used to match up spei data to particular treatment years
year.key <- edge_all %>%
  group_by(site, experiment.year, treatment.year, year) %>%
  summarise(year2 = unique(year)) 

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

# Merge FG Data ####
edge_FG.bootstrap <- left_join(response.ratio.tog.bootstrap, FG, by = "species") %>%
  filter(FunctionalGroup != "tree", !is.na(FunctionalGroup)) 

edge_FG.bootstrap$precip.bin <- as.factor(edge_FG.bootstrap$precip.bin)

edge_FG.bootstrap <- edge_FG.bootstrap %>%
  mutate(precip.bin = fct_relevel(precip.bin, "high", "med", "low"))

ggplot(edge_FG.bootstrap,
       aes(x=resp.ratio.site.mean_drought.RR,y=resp.ratio.site.mean_recovery.RR))+
  geom_point()+
  geom_errorbar(aes(ymin=resp.ratio.site.mean_recovery.RR-resp.ratio.site.margin_recovery.RR,
                    ymax=resp.ratio.site.mean_recovery.RR+resp.ratio.site.margin_recovery.RR))+
  geom_errorbarh(aes(xmin=resp.ratio.site.mean_drought.RR-resp.ratio.site.margin_drought.RR,
                    xmax=resp.ratio.site.mean_drought.RR+resp.ratio.site.margin_drought.RR))+
  facet_wrap(~precip.bin)


# Clean up ####
rm(edge_all, edge_w_zeros, rank_persist, resp.ratio.site, resp.ratio.site.recov, resp.ratio.yearly, response.ratio.tog, drought.RR.bootstrap, recov.RR.bootstrap, edge_w_predictors.site, edge_w_predictors.site.recov)
