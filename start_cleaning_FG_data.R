## Clean LTER data to extract needed functional groups

## read in functional group data
lterFG <- read.csv("data/LTER-grasslands-master-2012.csv")

## read in cleaned edge data
source("data-prep/clean_edge_data.R")

## explore
colnames(lterFG)
unique(lterFG$location)

## keep only FG data from lter sites
lterFG2 <- lterFG %>%
  mutate(genuscode = toupper(substr(species, 1, 3)), ## make a species code column
         sp.ep = strsplit(species, " ") %>%
           sapply(tail, 1),
         spepcode = toupper(substr(sp.ep, 1, 3)),
         spcode = paste0(genuscode, spepcode)) %>%
  select(-sitesubplot, -year, -experiment, - site, - plot, -subplot, -subplota, -abundance, -unitAbund, -genuscode, -sp.ep, -spepcode, -location) %>% ## drop unneeded columns
  distinct() ## keep distinct observations

#sp <- data.frame(species = unique(lterFG$species))
#test1 <- left_join(sp, lterFG2, by = "species")

## create a key at the species level
key <- lterFG2 %>%
  group_by(species, spcode) %>%
  summarise(FG = unique(growth), 
            LH = unique(duration)) %>%
  mutate(SPECIES = species) %>%
  ungroup() %>%
  mutate(genus = tolower(strsplit(species, " ")%>%
           sapply(head, 1)),
         sp.ep = strsplit(species, " ")%>%
           sapply(tail, 1)) %>%
  select(-species)

## create a key at the genus level
genus.key <- key %>%
  group_by(genus) %>%
  select(genus, FG)%>%
  distinct() %>%
  filter(FG != "")

## make genus column in edge_all dataframe
edge_all <- edge_all %>%
  mutate(genus = tolower(strsplit(species, "_")%>%
           sapply(head, 1)),
         sp.ep = strsplit(species, "_")%>%
           sapply(tail, 1))

t1 <- left_join(edge_all, genus.key, by = "genus")

na.check <- t1 %>%
  filter(is.na(FG)) %>%
  group_by(genus) %>%
  select(genus, sp.ep, FG) %>%
  distinct()
