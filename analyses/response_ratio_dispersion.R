# Header #### 
## Script name: Dispersions of drought ratios
##
## Purpose of script: Calculate dispersions for response ratios
##
## Author: Anny Chung
##
## Email: yyachung@uga.edu

# Set up env------------------------------------
library(car)
library(emmeans)

# Calculate the response ratios from other script
source("analyses/calculate_response_ratio.R")

theme_set(theme_classic())

## visualize
ggplot(edge_FG, aes(x=drought.RR, y=recovery.RR, color = FunctionalGroup)) +
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
edge_FG$dispersion <- sqrt(0.5*(edge_FG$drought.RR - edge_FG$recovery.RR)^2)

# Add strategy/quadrant information ----------
edge_FG <- edge_FG %>%
  #mutate(strategy = case_when(
   # resp.ratio.drought>0&resp.ratio.recov>0 ~ 'DroughtPosRecovPos',
    #resp.ratio.drought>0&resp.ratio.recov<0 ~ 'DroughtPosRecovNeg',
    #resp.ratio.drought<0&resp.ratio.recov>0 ~ 'DroughtNegRecovPos',
    #resp.ratio.drought<0&resp.ratio.recov<0 ~ 'DroughtNegRecovNeg'
  #)) #Any point that falls on zero for a response ratio will be NA
  mutate(quad = ifelse(drought.RR > 0 & recovery.RR > 0, "D+R+",
                       ifelse(drought.RR > 0 & recovery.RR < 0, "D+R-",
                              ifelse(drought.RR < 0 & recovery.RR < 0, "D-R-",
                                     ifelse(drought.RR < 0 & recovery.RR > 0,"D-R+", 
                                            ifelse(drought.RR == 0 & recovery.RR != 0, "recovered",
                                                   ifelse(drought.RR != 0 & recovery.RR == 0, "lost", 
                                                          ifelse(drought.RR == 0 & recovery.RR == 0, "no effect", NA))))))))

edge_FG$quad <- factor(edge_FG$quad, levels = c("D-R+", "D+R+", "D-R-", "D+R-", "lost", "recovered", "no effect"))

# Prelim visualizations with dispersion -----------

## dispersion by site
ggerrorplot(edge_FG,
            x="site",
            y="dispersion") +
  geom_point(shape = 1, position = "jitter", alpha = 0.2) +
  ylab("Dispersion") +
  xlab("Site")

ggsave("preliminary_figs/meeting_jan_2024/dispersion_site.png", width = 6, height = 4)

## by functional group and site
ggerrorplot(edge_FG,
            x= "FunctionalGroup",
            y= "dispersion",
            facet.by = "site")+ 
  geom_point(shape = 1, position = "jitter", alpha = 0.2) +
  ylab("Dispersion") +
  xlab("Functional Group")

ggsave("preliminary_figs/meeting_jan_2024/dispersion_site_FG.png", width = 6, height = 4)

## add by strategy  
ggerrorplot(edge_FG,
            x= "quad",
            y= "dispersion",
            color = "FunctionalGroup",
            facet.by = "site")+ rotate_x_text(45) +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606"))

ggsave("preliminary_figs/meeting_jan_2024/dispersion_site_FG_quad.png", width = 6, height = 4)


## by rank and functional group and site
ggplot(edge_FG, aes(x=percrank, y=dispersion, color = FunctionalGroup)) +
  geom_point(shape = 20, size = 2) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  xlab("Rank") +
  ylab("Dispersion from 1:1 response line") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606"))

ggsave("preliminary_figs/meeting_jan_2024/dispersion_rank_FG.png", width = 5.5, height = 4)

## by persistence and functional group and site
ggplot(edge_FG, aes(x=persistence.site, y=dispersion, color = FunctionalGroup)) +
  geom_point(shape = 20, size = 2) +
  geom_smooth(aes(color = FunctionalGroup), method = "lm", alpha = 0, linewidth = 0.8) +
  facet_wrap(~site, nrow = 3, ncol = 2) +
  xlab("Persistence") +
  ylab("Dispersion from 1:1 response line") +
  scale_color_manual(values = c("#CC61B0", "#99C945","#5D69B1", "#E58606"))

ggsave("preliminary_figs/meeting_jan_2024/dispersion_persist_FG.png", width = 5.5, height = 4)

# Do some stats on dispersion -----------

## Does dispersion depend on site and/or functional group and persistence?
m1 <- lm(dispersion~persistence.site*site*FunctionalGroup, data=edge_FG)
qqPlot(m1) #pretty good
plot(m1) #really heteroscedastic residuals...
hist(m1$residuals)
Anova(m1) 
### Persistence (P<0.001) and site (P=0.017) are significant
### Functional Group is p=0.08
### No significant interactions
pairs(emmeans(m1,~site, adjust = "sidak"))
### Only sig contrast is SEV_black vs. Hays
pairs(emtrends(m1,"site", "persistence.site"))
### No difference in persistence slopes among sites

## Does dispersion depend on site and/or functional group and rank?
m2 <- lm(dispersion~percrank*site*FunctionalGroup, data=edge_FG)
qqPlot(m2) #pretty good
plot(m2) #really heteroscedastic residuals...
hist(m2$residuals)
Anova(m2) 
### Rank (P<0.001) and site (P<0.001) are significant
### Functional Group is also significant (P=0.002)
### Rank:Site P=0.07
pairs(emmeans(m2,~site, adjust = "sidak"))
### Sig contrasts is SEV_black vs. KNZ, Hays, CHY, SGS, and SEV_blue
pairs(emmeans(m2,~FunctionalGroup, adjust = "sidak"))
### Grass > Forb >= Shrub
pairs(emtrends(m2,"site", "percrank"))
### SEV_blue and SEV_black sig different slopes
