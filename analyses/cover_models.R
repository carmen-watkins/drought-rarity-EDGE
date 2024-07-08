## Cover Models
source("analyses/cov_thru_time.R")

library(emmeans)

# Figure 2 Models ####
## For data in each panel, run a model lm(cover~year*treatment), make sure year is a factor, not continuous. Then, posthoc (use emmeans) test for pairwise comparisons between drought and control for each year. Mark significant comparisons with a star

## change year to a factor for model comparisons
edge_sum_plot$year <- as.factor(edge_sum_plot$year)

signif_out <- data.frame(site = NA, sp.type = NA, comparison = NA, p_value = NA)
all_out <- data.frame(site = NA, sp.type = NA, comparison = NA, p_value = NA)

site <- unique(edge_sum_plot$site)
rarity <- unique(edge_sum_plot$rarity)

## iterate over all sites & sp types
for(i in 1:length(site)) {
  
  s = site[i]
  
  for(j in 1:length(rarity)) {
    
    r = rarity[j]
    
    ## filter data
    tdat <- edge_sum_plot %>%
      filter(site == s, ## select site
             rarity == r) ## select sp type
    
    ## run linear model
    tm <- lm(mean.cov~year*treatment, data = tdat)
    
    ## do emmeans to ______
    tfit <- emmeans(tm, c("year", "treatment"), data = tdat)
    
    ## look at post-hoc comparisons
    tpairs <- pairs(tfit, adjust = "tukey")
    
    ## put into df
    tmp_all <- data.frame(comparison = summary(tpairs)$contrast, p_value = summary(tpairs)$p.value) %>%
      mutate(site = site[i],
             sp.type = rarity[j])
    
    ## filter out signif comparisons
    tmp_signif <- tmp_all %>%
      filter(p_value < 0.05,
             substr(comparison, start = 8, stop = 8) == substr(comparison, start = 21, stop = 21))
   
    ## append
    signif_out <- rbind(signif_out, tmp_signif)
    all_out <- rbind(all_out, tmp_all)
     
  }
  
}


knz <- signif_out[signif_out$site == "KNZ",]
hys <- signif_out[signif_out$site == "HYS",]
chy <- signif_out[signif_out$site == "CHY",]
sgs <- signif_out[signif_out$site == "SGS",]
sbk <- signif_out[signif_out$site == "SBK",]
sbl <- signif_out[signif_out$site == "SBL",]

# Figure S4 Models ####

# Figure S5 Models ####
