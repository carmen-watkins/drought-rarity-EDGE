source("Relativecover_Richness_ID_HF.R")

HFedgeallclass1$RR<-
  
  
ResponseRatio<-HFedgeallclass1 %>% 
  group_by(site,species,treatment) %>%
  summarize(totalcover= sum(max.cover))


ResponseRatio$RR<-ResponseRatio$totalcover/ResponseRatio$totalcover, group_by(species,site,treatment)


HFedgeallclass1$relcover<-HFedgeallclass1$max.cover/HFedgeallclass1$totalcover

