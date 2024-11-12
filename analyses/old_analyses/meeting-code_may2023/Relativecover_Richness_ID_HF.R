#Sum of the 
source("data-prep/join_control_edge_data.R")
library(dplyr)
RichbyCat<-edge_all_class %>%
  select(c(site,species,nickname)) %>%
  group_by(site,nickname) %>%
  dplyr::summarize(SR=length(unique(species,na.rm=T)))

IDbyCat<-edge_all_class %>%
  select(c(site,species,nickname)) %>%
  group_by(site,nickname) %>%
  dplyr::summarize(ID=unique(species,na.rm=T))

SumID<-IDbyCat %>% 
  group_by(site, nickname) %>% 
  summarise(ID = list(ID))%>%
  
HFedgeallclass<-edge_all_class

datatotalcover<-HFedgeallclass %>% 
  group_by(site,plot,subplot,year,treatment) %>%
  summarize(totalcover= sum(max.cover))

HFedgeallclass1 <- left_join(HFedgeallclass, datatotalcover, by=c('site'='site','plot'='plot','subplot'='subplot','year'='year','treatment'='treatment'))
HFedgeallclass1$relcover<-HFedgeallclass1$max.cover/HFedgeallclass1$totalcover
