

## Clean FG Data

## Taxa info data sheet: sent from Kathy Condon on 1/3/24
## PPS011: KNZ species list with functional group info; downloaded from https://doi.org/10.6073/pasta/50f594d82bc0b385406662fb5dcba59f
## SEV data downloaded from online repo; contains functional group info
## Sevilleta Species List 2019: sent from Anny Chung

knz_sp = read.csv("data/functional_group_original/PPS011.csv")
north_sp = read.csv("data/functional_group_original/Taxa_Info_original.csv")
sev_sp = read.csv("data/functional_group_original/SevilletaSpeciesList2019.csv")


## updated data from Cristy & Beatriz
edge_fg = read.csv("data/edge_species_info_CP_BA.csv")

names(knz_sp)
names(north_sp)
names(sev_sp)

## join KNZ and North sp first

## modify north sp to fit formatting
names(north_sp) = c("code", "family", "genus", "species", "photo", "growthform")


# 1. Update KNZ w/North ####
## Left join north + knz to find common species with filled in info
##prep north for join
north_sp2 = north_sp %>%
  mutate(genus = tolower(genus),
         family = tolower(family),
         photoN = tolower(photo)) %>%
  select(-photo)

## join
knz_sp2 = left_join(knz_sp, north_sp2, by = c("family", "genus", "species"))  %>%
  mutate(photo_tog = ifelse(photo == photoN | is.na(photoN), photo,
                            ifelse(photo == ".", photoN,
                                   "other"))) %>%
  ## fix two other instances - they are clearly C4's but were wrong in Taxa Info spreadsheet
  
  mutate(photo_tog = ifelse(code.y %in% c("SCSC", "SONU"), "c4", photo_tog))

## clean df 
knz_sp_up = knz_sp2 %>%
  select(-updatedyear, -gen, -spec, -growthform.y, -photoN, -photo, -Comments, -code.x, -code.y) %>%
  filter(genus != ".") %>%
  mutate(growthform = growthform.x,
         full_name = paste0(genus, "_", species)) %>%
  select(-growthform.x)

# 2. Join updt KNZ + north unique ####
## need to check if there are species from north df not in knz df
## create list of north species
north_list = north_sp2 %>%
  mutate(full_name = paste0(genus, "_", species)) %>%
  select(full_name) %>%
  distinct()

not_on_knz = north_list %>%
  filter(!full_name %in% knz_sp_up$full_name)

## make df of unique north species
north_unique = north_sp2 %>%
  mutate(full_name = paste0(genus, "_", species)) %>%
  filter(full_name %in% not_on_knz$full_name, 
         genus != "unk") %>%
  mutate(lifespan = NA, 
         origin = NA, 
         photo_tog = photoN) %>%
  select(-photoN, -code)

## join KNZ + north unique
north_all = rbind(knz_sp_up, north_unique) %>%
  mutate(growthform = ifelse(growthform == "grass", "g",
                             ifelse(growthform == "forb", "f", 
                                    ifelse(growthform == "sedge", "s",
                                           ifelse(growthform == "shrub", "w", growthform)))),
         
         lifespan = tolower(lifespan)) %>%
  distinct()

# 3. update info with SEV ####
sev_overlap = sev_sp %>%
  mutate(genus = tolower(genus),
         full_name = paste0(genus, "_", sp_epithet)) %>%
  filter(full_name %in% north_sp_list) 

## join in sev data to make sure all the FG/duration data is shared
north_all_sev_info = left_join(north_all, sev_overlap, by = c("full_name", "genus")) %>%
  
  mutate(FunctionalGroup = ifelse(FunctionalGroup %in% c("s", "t"), "w", FunctionalGroup)) %>%
  
  mutate(PhotoPath = tolower(PhotoPath),
         photo_tog2 = ifelse(photo_tog == PhotoPath | is.na(PhotoPath), photo_tog,
                                         ifelse(photo_tog == ".", PhotoPath,
                                                "other")),
         
         fg_tog = ifelse(growthform == FunctionalGroup | is.na(FunctionalGroup), growthform,
                         ifelse(growthform == ".", FunctionalGroup,
                                "other")), 
         
         duration_tog = ifelse(lifespan == LifeHistory | is.na(LifeHistory), lifespan,
                               ifelse(lifespan == ".", LifeHistory,
                                      "other")),
         family = family.x) %>%
  select(-family.y, -family.x) %>%
  select(full_name, family, genus, species, fg_tog, duration_tog, photo_tog2)


unique(north_all_sev_info$fg_tog)
unique(north_all_sev_info$duration_tog)
unique(north_all_sev_info$photo_tog)

# 4. Join in unique SEV sp ####
sev_sp_unique = sev_sp %>%
  mutate(genus = tolower(genus),
         full_name = paste0(genus, "_", sp_epithet),
         species = sp_epithet,
         photo_tog2 = tolower(PhotoPath), 
         duration_tog = LifeHistory,
         fg_tog = FunctionalGroup) %>%
  filter(!full_name %in% north_all_sev_info$full_name) %>%
  select(full_name, family, genus, species, fg_tog, duration_tog, photo_tog2)


fg_all = rbind(north_all_sev_info, sev_sp_unique) %>%
  distinct() %>%
  filter(!(full_name == "heterotheca_villosa" & is.na(photo_tog2)),
         !(full_name == "lepidium_densiflorum" & photo_tog2 == "other")) 


# 5. Join to RR data ####
edge_RR2 <- edge_RR %>%
  mutate(full_name = tolower(species)) %>%
  left_join(fg_all, by = "full_name")





## all edge species
edge_sp_all = edge_RR2 %>%
  select(full_name) %>%
  distinct()

##want to join in old edge_fg data 

##prep edge_fg
edge_fg2 = edge_fg %>%
  mutate(full_name = tolower(species)) %>%
  select(genus, full_name, FunctionalGroup, Duration, Photo) %>%
  distinct()


t1 = left_join(edge_sp_all, edge_fg2, by = c("full_name"))

t2 = left_join(t1, fg_all, by = c("full_name", "genus")) %>%

  mutate(fg_tog2 = ifelse(fg_tog == "f", "forb",
                         ifelse(fg_tog %in% c("g", "s"), "grass", fg_tog)),
         fg_tog2 = ifelse(full_name == "eriogonum_effusum", "w", fg_tog2)) %>%
  
  mutate(fg_final = ifelse(fg_tog2 == FunctionalGroup, FunctionalGroup,
                           
                           ifelse(fg_tog2 %in% c("w", "m"), FunctionalGroup,
                                  
                                  ifelse( (fg_tog2 == "forb" & FunctionalGroup == "shrub"), "forb",
                                          
                                          ifelse(fg_tog2 == "NA", FunctionalGroup, "NONE")
                                          
                                          )))) %>%
  mutate(photo_final = Photo, 
         photo_final = ifelse(photo_final %in% c("", "unk"), photo_tog2, photo_final))




table(edge_RR2$photo_tog2)

ggplot(edge_RR2, aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  facet_wrap(~photo_tog2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm")

ggplot(edge_RR2, aes(x=spatial_rarity, y=resp.ratio.site_D4)) +
  geom_point() +
  facet_wrap(~fg_tog) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "lm")


table(edge_RR2$fg_tog)


ggplot(edge_RR2, aes(x=site, fill = photo_tog2)) +
  geom_bar()



