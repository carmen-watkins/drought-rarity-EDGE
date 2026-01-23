# Header #### 
## Script name: Clean Aridity Data
##
## Purpose of script: Prep aridity data for analyses.
##
## Author: Carmen Watkins
##

aridity = read.csv("data/aridity_index_results.csv")

ggplot(aridity, aes(x=AI_Actual, y=site_name)) + 
  geom_point()

aridity
aridity_clean = aridity %>%
  ## change site codes to match rest of data
  mutate(site = ifelse(site_code == "HAR", "HYS",
                       ifelse(site_code == "HPG", "CHY",
                              ifelse(site_code == "CPR", "SGS",
                                     ifelse(site_code == "SEVB", "SBL",
                                            ifelse(site_code == "SEVG", "SBK", 
                                                   site_code))))),
         aridity = AI_Actual) %>%
  select(site, aridity)

## clean environment 
rm(aridity)