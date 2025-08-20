## explore 6-year drought response ratio
source("analyses/calculate_response_ratio.R")

## merge data frames
### first, add drought length variable
drought.SE.RII.6 = drought.SE.RII.6 %>%
  mutate(dlength = 6)

drought.SE.RII = drought.SE.RII %>%
  mutate(dlength = 4)

## merge
SEV_check = rbind(drought.SE.RII.6, drought.SE.RII) %>%
  filter(site %in% c("SBK", "SBL"))

## merge with rank/persistence data
SEV_check2 = left_join(SEV_check, rank_persist, by = c("site", "species"))

## summarise data for visualization
sum_SEV = SEV_check2 %>%
  group_by(site, species, dlength) %>%
  summarise(meanDRR = mean(resp.ratio.site, na.rm = T), 
            seDRR = calcSE(resp.ratio.site),
            persistence = median(persistence.site), 
            rank = median(percrank)) %>%
  
  mutate(spatial = ifelse(rank > 0.5, "Abundant", "Scarce"),
         temporal = ifelse(persistence > 0.5, "Core", "Transient"),
         rarity_cat = paste0(temporal, ", ", spatial))
       
ggplot(sum_SEV, aes(x=rank, y=meanDRR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~dlength) +
  xlab("Spatial Rarity (rank)") +
  ylab("Drought Response Ratio")

ggsave("DRR_rank_SEV_4_6.png", width = 6, height = 3)

ggplot(sum_SEV, aes(x=persistence, y=meanDRR)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~dlength) +
  xlab("Temporal Rarity (persistence)") +
  ylab("Drought Response Ratio")

ggsave("DRR_persistence_SEV_4_6.png", width = 6, height = 3)

