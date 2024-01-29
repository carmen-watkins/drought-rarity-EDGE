# Header #### 
## Script name: Dispersions of drought ratios
##
## Purpose of script: Calculate dispersions for response ratios
##
## Author: Anny Chung
##
## Email: yyachung@uga.edu

# Set up env------------------------------------

# Calculate the response ratios from other script
source("analyses/calculate_response_ratio.R")

# Read in functional group data
FG <- read.csv("data/edge_species_info.csv")

theme_set(theme_classic())

# Join FG Data and visualize -------------------
edge_temp <- edge_w_predictors.site %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG <- left_join(edge_temp, FG, by = "species")

edge_temp2 <- edge_w_predictors.site.recov %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
                           sapply(head, 1)))

edge_FG_recov <- left_join(edge_temp2, FG, by = "species")

drought.RR <- edge_FG %>%
  mutate(resp.ratio.drought = resp.ratio.site,
         mean.cov.drought = mean.cov) %>%
  select(site, species, resp.ratio.drought, mean.cov.drought, persistence.site, percrank, FunctionalGroup)

recov.RR <- edge_FG_recov %>%
  mutate(resp.ratio.recov = resp.ratio.site,
         mean.cov.recov = mean.cov) %>%
  select(site, species, resp.ratio.recov, mean.cov.recov, FunctionalGroup)

## merge drought & recov dataframes
response.ratio.tog <- left_join(drought.RR, recov.RR, by = c("site", "species", "FunctionalGroup")) %>%
  mutate(precip.bin = ifelse(site %in% c("KNZ", "HYS"), "high",
                             ifelse(site %in% c("CHY", "SGS"), "med", "low")))

## make sure NAs are zeroes -- sorry this is in ugly base R
response.ratio.tog$resp.ratio.recov[is.na(response.ratio.tog$resp.ratio.recov)] <- 0
response.ratio.tog$mean.cov.recov[is.na(response.ratio.tog$mean.cov.recov)] <- 0

response.ratio.tog$resp.ratio.drought[is.na(response.ratio.tog$resp.ratio.drought)] <- 0
response.ratio.tog$mean.cov.drought[is.na(response.ratio.tog$mean.cov.drought)] <- 0

response.ratio.tog$precip.bin <- as.factor(response.ratio.tog$precip.bin)

response.ratio.tog <- response.ratio.tog %>%
  mutate(precip.bin = fct_relevel(precip.bin, "high", "med", "low"))

## visualize
ggplot(response.ratio.tog, aes(x=resp.ratio.drought, resp.ratio.recov, color = FunctionalGroup)) +
  geom_hline(yintercept = 0, color = "grey", linewidth = 0.25) +
  geom_vline(xintercept = 0, color = "grey", linewidth = 0.25) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  #geom_smooth(method = "lm", alpha = 0.10, color = "black", linewidth = 0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Drought Response Ratio") +
  ylab("Recovery Response Ratio") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606"))

# Calculate the dispersion---------------------
# Dispersion is defined here as the perpendicular absolute distance of each point to the 1:1 line
response.ratio.tog$dispersion <- sqrt(0.5*(response.ratio.tog$resp.ratio.drought - response.ratio.tog$resp.ratio.recov)^2)

# Add strategy/quadrant information ----------
response.ratio.tog <- 
response.ratio.tog %>%
  mutate(strategy = case_when(
    resp.ratio.drought>0&resp.ratio.recov>0 ~ 'DroughtPosRecovPos',
    resp.ratio.drought>0&resp.ratio.recov<0 ~ 'DroughtPosRecovNeg',
    resp.ratio.drought<0&resp.ratio.recov>0 ~ 'DroughtNegRecovPos',
    resp.ratio.drought<0&resp.ratio.recov<0 ~ 'DroughtNegRecovNeg'
  )) #Any point that falls on zero for a response ratio will be NA

# Prelim visualizations with dispersion -----------
## by functional group and site
ggerrorplot(response.ratio.tog,
            x= "FunctionalGroup",
            y= "dispersion",
            facet.by = "site")

## by strategy and site 
ggerrorplot(response.ratio.tog,
            x= "strategy",
            y= "dispersion",
            facet.by = "site")+ rotate_x_text(45)

## by rank and functional group and site
ggplot(response.ratio.tog, aes(x=percrank, y=dispersion, color = FunctionalGroup)) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  xlab("Rank") +
  ylab("Dispersion from 1:1 response line") 

## by persistence and functional group and site
ggplot(response.ratio.tog, aes(x=persistence.site, y=dispersion, color = FunctionalGroup)) +
  geom_point(shape = 20, size = 2) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  xlab("Persistence") +
  ylab("Dispersion from 1:1 response line") 
