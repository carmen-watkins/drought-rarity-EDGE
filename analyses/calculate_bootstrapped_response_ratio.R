# Header #### 
## Script name: Calculate Boot strapped Response Ratio
##
## Purpose of script: Calculate the response ratio between drought and control plots 1. across years during two time periods (drought & recovery) and 2. for each year. Add in functional group data as well.
## Code version only bootstraps species-site-treatment combinations that reach a min number of measurements in EITHER the treatment or control plots
##
## Author: Carmen Watkins & Lukas Bell-Dereske
##
## Email: cebel2@uoregon.edu

# Set up env ####
## read in cleaned data
source("data-prep/classify_rank_persistence.R")
#source("data-prep/clean_edge_data.R")

FG <- read.csv("data/edge_species_info.csv")

"%w/o%" <- function(x,y)!('%in%'(x,y))# a function for the opposite of "%in%", not in list

# Resp Ratio Across Years ####
#Function to create a response ratio with random plant measures removed
random_spp_mat_drought <- function(orig_mat, #tibble created earlier with the cleaned plant data
                                   frac_samp=NULL, #proportion of plant measure you want to randomly exclude without replacement
                                   min_samples=NULL #minimum number of samples for a species that you would like to include in bootstrapping
                                   ){
  resp.ratio.site.temp <- orig_mat %>%
    filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
    group_by(site, treatment, species) %>%
    summarise(total.samps=n())%>%
    filter(total.samps>=min_samples) %>%
    ungroup()%>%
    distinct(site,species)%>%
    left_join(orig_mat,
              join_by(site,species))%>%
    filter(experiment.year %in% c(1:4)) %>% 
    group_by(site, species) %>%
    slice_sample(prop = (1-frac_samp))%>% #this is the randomly sampling step
    group_by(site, treatment, species) %>%
    summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
    pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    group_by(site, species) %>%
    mutate(resp.ratio.site = (D-C)/(C+D)) 
  
  return(resp.ratio.site.temp)

}

#Function for the random re-sampling/bootstrapping that use the function "random_spp_mat_drought"
resp.ratio.site.bootstrap.df <- function(orig_dibble,#tibble created earlier with the cleaned plant data
                                         frac_samp=NULL, #proportion (decimal) of plant measure you want to randomly exclude without replacement
                                         min_samples=NULL, #minimum number of samples for a species that you would like to include in bootstrapping
                                         boots_perm=1 #number or permutations that you would like to conduct
                                         ){
  
  random_site_ratio_temp<-data.frame("int_num"= as.numeric(),
                                "site" = as.character(),
                                "species" = as.character(),
                                "C" = as.numeric(),
                                "D" = as.numeric(), 
                                "resp.ratio.site" = as.numeric())
  for (i in 1:boots_perm) { #the for loop that conducts the bootstrapping
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
  random_site_ratio_temp2<- #adds back in the species that did not make the minimum number of samples. >= "min_samples"
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
}# this code with return a tibble with values for every iteration conducted
# I decided to return this large tibble format for flexibility in summarizing and calculating variance




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


#Run of the bootstrapping with my minimum parameters
#We could change these if we want to be more conservative
resp.ratio.site_bootstrapped<-resp.ratio.site.bootstrap.df(edge_all, 
                                            frac_samp=0.2, 
                                            min_samples=4,
                                            boots_perm=100)#100 permutations returns 27,439 rows in the tibble

#Species that do not reach the min of 4 measures#####
print(resp.ratio.site_bootstrapped|>
  filter(is.na(int_num)),n=139)

lowrep.sp <- resp.ratio.site_bootstrapped|>
  filter(is.na(int_num))

#Summarizing the permutation data for mean and 95% confidence
resp.ratio.site_bootstrapped_sum<- 
  resp.ratio.site_bootstrapped%>%
  group_by(site, species)%>%
  summarise(resp.ratio.site.mean=mean(resp.ratio.site),
            resp.ratio.site.median=median(resp.ratio.site),
            resp.ratio.site.margin=qt(0.975,df=n()-1)*sd(resp.ratio.site)/sqrt(n()))#NAs are produced due to the non-bootstrapped species



## merge with rank and persistence values for each species
edge_w_predictors.site <- left_join(resp.ratio.site, rank_persist, by = c("site", "species"))#original dataset for sanity checks
edge_w_predictors.site.bootstrap <- left_join(resp.ratio.site_bootstrapped_sum, rank_persist, by = c("site", "species"))

## change site to an ordered factor
edge_w_predictors.site$site <- as.factor(edge_w_predictors.site$site)#original dataset for sanity checks
edge_w_predictors.site <- edge_w_predictors.site %>%#original dataset for sanity checks
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))
dim(edge_w_predictors.site)
#412   9

edge_w_predictors.site.bootstrap$site <- as.factor(edge_w_predictors.site.bootstrap$site)
edge_w_predictors.site.bootstrap <- edge_w_predictors.site.bootstrap %>%
  mutate(site = fct_relevel(site, c("KNZ", "HYS", "CHY", "SGS", "SEV_blue", "SEV_black")))
dim(edge_w_predictors.site.bootstrap)
#412   9



#Sanity check
#Let's see how well the bootstrapped results match the original calculations 


edge_w_predictors.site.bootstrap_TEMP<-
  inner_join(edge_w_predictors.site,edge_w_predictors.site.bootstrap)

#Graph of the regression
ggplot(edge_w_predictors.site.bootstrap_TEMP,
       aes(x=resp.ratio.site,
           y=resp.ratio.site.mean))+
  geom_point()+
  geom_errorbar(aes(ymin=resp.ratio.site.mean-resp.ratio.site.margin,
                    ymax=resp.ratio.site.mean+resp.ratio.site.margin))+
  annotate("label", x=-0.5,y=1, 
           label=paste("Pearson r =",signif(cor(edge_w_predictors.site.bootstrap_TEMP$resp.ratio.site,
                                               edge_w_predictors.site.bootstrap_TEMP$resp.ratio.site.mean,
                                               method = "pearson"),3)))+ #Including the correlation in the plot
  theme_bw()

ggsave("preliminary_figs/march_2024/bootstrap_DRR_sanity_check.png", width = 4, height = 4)

## During Recovery ####
### Site Level ####
#Function to create a response ratio with random plant measures removed
random_spp_mat_recov <- function(orig_mat, #tibble created earlier with the cleaned plant data
                                 frac_samp=NULL, #proportion of plant measure you want to randomly exclude without replacement
                                 min_samples=NULL #minimum number of samples for a species that you would like to include in bootstrapping
                                 ){
  resp.ratio.site.temp <- orig_mat %>%
    filter(treatment.year == "recovery") %>% 
    group_by(site, treatment, species) %>%
    summarise(total.samps=n())%>%
    filter(total.samps>=min_samples) %>%
    ungroup()%>%
    distinct(site,species)%>%
    left_join(orig_mat,
              join_by(site,species))%>%
    filter(treatment.year == "recovery") %>%
    group_by(site, species) %>%
    slice_sample(prop = (1-frac_samp))%>%#this is the randomly sampling step
    group_by(site, treatment, species) %>%
    summarise(mean.cover.sp = mean(mean.plot.cover)) %>% ## mean cover by site across years
    pivot_wider(names_from = "treatment", values_from = "mean.cover.sp") %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    group_by(site, species) %>%
    mutate(resp.ratio.site = (D-C)/(C+D)) 
  
  
  return(resp.ratio.site.temp)
  
  
}


#Function for the random re-sampling/bootstrapping that use the function "random_spp_mat_recov"

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


#Run of the bootstrapping with my minimum parameters
#We could change these if we want to be more conservative

resp.ratio.site.recov_bootstrapped<-resp.ratio.site.recov.bootstrap.df(edge_all, 
                                                           frac_samp=0.2, 
                                                           min_samples=4,
                                                           boots_perm=100)

#Species that do not reach the min of 4 measures#####
print(resp.ratio.site.recov_bootstrapped|>
        filter(is.na(int_num)),n=135)

#Summarizing the permutation data for mean and 95% confidence
resp.ratio.site.recov_bootstrapped_sum<- 
  resp.ratio.site.recov_bootstrapped%>%
  group_by(site, species)%>%
  summarise(resp.ratio.site.mean=mean(resp.ratio.site),
            resp.ratio.site.median=median(resp.ratio.site),
            resp.ratio.site.margin=qt(0.975,df=n()-1)*sd(resp.ratio.site)/sqrt(n())) #NAs produced for the species with no bootstrap values

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

#Sanity check
#Let's see how well the bootstrapped results match the original calculations 


edge_w_predictors.site.recov.bootstrap_TEMP<-
  inner_join(edge_w_predictors.site.recov,edge_w_predictors.site.recov.bootstrap)

#Graph of the regression
ggplot(edge_w_predictors.site.recov.bootstrap_TEMP,
       aes(x=resp.ratio.site,
           y=resp.ratio.site.mean))+
  geom_point()+
  geom_errorbar(aes(ymin=resp.ratio.site.mean-resp.ratio.site.margin,
                    ymax=resp.ratio.site.mean+resp.ratio.site.margin))+
  annotate("label", x=-0.5,y=1, 
           label=paste("Pearson r =",signif(cor(edge_w_predictors.site.recov.bootstrap_TEMP$resp.ratio.site,
                                                edge_w_predictors.site.recov.bootstrap_TEMP$resp.ratio.site.mean,
                                                method = "pearson"),3)))+ #Including the correlation in the plot
  theme_bw()

ggsave("preliminary_figs/march_2024/bootstrap_RRR_sanity_check.png", width = 4, height = 4)

###NOTE: I did not write bootstrapping code for the yearly. 
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
#list of objects to be removed 

rm(edge_all, edge_w_zeros, rank_persist, resp.ratio.site, resp.ratio.site.recov, resp.ratio.yearly, response.ratio.tog, drought.RR.bootstrap, recov.RR.bootstrap, edge_w_predictors.site, edge_w_predictors.site.recov)
