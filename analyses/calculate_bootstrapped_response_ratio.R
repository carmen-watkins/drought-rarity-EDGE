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

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# a function for the opposite of "%in%", not in list
"%w/o%" <- function(x,y)!('%in%'(x,y))

# Drought ####
## Create functions ####
#Function to create a response ratio with random plant measures removed
random_spp_mat_drought <- function(orig_mat, #tibble created earlier with the cleaned plant data
                                   frac_samp=NULL, #proportion of plant measure you want to randomly exclude without replacement
                                   min_samples=NULL #minimum number of samples for a species that you would like to include in bootstrapping
                                   ){
  resp.ratio.site.temp <- orig_mat %>%
    filter(experiment.year %in% c(1:4)) %>% ## 0 is pre-treat year; drought was years 1-4
    group_by(site, species) %>%
    summarise(total.samps=n())%>%
    filter(total.samps>=min_samples) %>% ## this is the min samples across treatments
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

## Run bootstrap ####
#Run of the bootstrapping with my minimum parameters
resp.ratio.site_bootstrapped<-resp.ratio.site.bootstrap.df(edge_all, 
                                            frac_samp=0.2, 
                                            min_samples=4,
                                            boots_perm=100)#100 permutations returns 27,439 rows in the tibble

### Species below min 4 measures 
print(resp.ratio.site_bootstrapped|>
  filter(is.na(int_num)),n=139)

lowrep.sp <- resp.ratio.site_bootstrapped|>
  filter(is.na(int_num))

## Summarise permutation ####
#Summarizing the permutation data for mean and 95% confidence
resp.ratio.site_bootstrapped_sum<- 
  resp.ratio.site_bootstrapped%>%
  group_by(site, species)%>%
  summarise(resp.ratio.site.mean=mean(resp.ratio.site),
            resp.ratio.site.median=median(resp.ratio.site),
            resp.ratio.site.margin=qt(0.975,df=n()-1)*sd(resp.ratio.site)/sqrt(n()),#NAs are produced due to the non-bootstrapped species

            resp.ratio.se = calcSE(resp.ratio.site))


# Recovery ####
## Create Functions ####
#Function to create a matrix containing some fraction of the data and calculate the response ratio
random_spp_mat_recov <- function(orig_mat, #tibble created earlier with the cleaned plant data
                                 frac_samp=NULL, #proportion of plant measure you want to randomly exclude without replacement
                                 min_samples=NULL #minimum number of samples for a species that you would like to include in bootstrapping
                                 ){
  resp.ratio.site.temp <- orig_mat %>%
    filter(treatment.year == "recovery") %>% 
    group_by(site, species) %>%
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

## Run Bootstrap ####
#Run of the bootstrapping with my minimum parameters
#We could change these if we want to be more conservative
resp.ratio.site.recov_bootstrapped<-resp.ratio.site.recov.bootstrap.df(edge_all, 
                                                           frac_samp=0.2, 
                                                           min_samples=4,
                                                           boots_perm=100)

## Species below min 4 measures 
print(resp.ratio.site.recov_bootstrapped|>
        filter(is.na(int_num)),n=135)

## Summarise permutation ####
#Summarizing the permutation data for mean and 95% confidence
resp.ratio.site.recov_bootstrapped_sum<- 
  resp.ratio.site.recov_bootstrapped%>%
  group_by(site, species)%>%
  summarise(resp.ratio.site.mean=mean(resp.ratio.site),
            resp.ratio.site.median=median(resp.ratio.site),
            resp.ratio.site.margin=qt(0.975,df=n()-1)*sd(resp.ratio.site)/sqrt(n()), #NAs produced for the species with no bootstrap values
            resp.ratio.se = calcSE(resp.ratio.site))

# Clean up ####
rm(edge_all, edge_w_zeros, rank_persist, resp.ratio.site, resp.ratio.site.recov, resp.ratio.yearly, response.ratio.tog, drought.RR.bootstrap, recov.RR.bootstrap, edge_w_predictors.site, edge_w_predictors.site.recov)
