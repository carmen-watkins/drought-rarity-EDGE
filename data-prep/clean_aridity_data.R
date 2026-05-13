# Header #### 
## Script name: Clean Aridity Data
##
## Purpose of script: Prep aridity data for analyses.
##
## Author: Carmen Watkins
##
## Aridity data were obtained from Zomer et al. (2022). 
## Version 3 of the Global Aridity Index and Potential Evapotranspiration 
## Database. Scientific Data, 9(1), 409. 
## https://doi.org/10.1038/s41597-022-01493-1

## read in data
aridity = read.csv("data/site_and_env_data/aridity_index_results.csv")

## plot
ggplot(aridity, aes(x=AI_Actual, y=site_name)) + 
  geom_point()

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