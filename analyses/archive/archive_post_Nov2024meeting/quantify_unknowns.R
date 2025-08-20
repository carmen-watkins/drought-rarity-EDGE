

source("data-prep/clean_cover_dat_sp_names.R")

all_unks = rbind(knz_unks, hys_unks, chy_unks, sgs_unks) %>%
  filter(max.cover > 0)

sbk_unks

nrow(all_unks) + nrow(sbk_unks)

median(all_unks$max.cover)
range(all_unks$max.cover)

nrow(all_unks[all_unks$max.cover < 4,]) / (nrow(all_unks) + nrow(sbk_unks))
