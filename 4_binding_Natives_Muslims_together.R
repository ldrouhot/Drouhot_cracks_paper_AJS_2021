
## Below, I have left the custom names of the data files used in Drouhot 2021 intact.

TeO.combined <- bind_rows(myTeO.native,myTeO.muslim,myTeO.nonnative)

rm(myTeO.native,myTeO.muslim,myTeO.nonnative)

# Full data 

TeO.full <- TeO.combined %>% 
left_join(TeO %>%dplyr::select(-c(sexee,urban_cluster,situpro_cluster_work,religiositysum)), by="ident")

## Adding module on discrimination originally left out during initial cleaning in Stata

paste(path.data,"indiv2_&_relig_&_context_pre_clustering.dta",sep="")

TeO_discri <- read.dta13(paste(path.data,"indiv2_&_relig_&_context_merged_d_discri.dta",sep=""), convert.factors = FALSE) %>% as_tibble()

TeO.full <- TeO.full %>% right_join(TeO_discri, by="ident")

name <- paste0("TeO_full_post_fuzzy_clustering.dta")

write_dta(TeO.full, name) # Writing dataset with clustering results across iterations. 