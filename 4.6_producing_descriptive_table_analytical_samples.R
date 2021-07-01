TeO.full <- read.dta13(paste(path.data,"TeO_full_post_fuzzy_clustering_051920.dta",sep=""), convert.factors = FALSE) %>% as_tibble()

## Above, this is restoring the dataset obtained after the membership coefficients have been obtained. 

################################################# PRODUCING GRAND MEANS ACROSS CLUSTERING ASSIGNMENTS AND TABLE ############################################
#
# THIS CODE IS FOR A GENERAL DESCRIPTIVE TABLE OF VARIABLES OF INTEREST.

TeO.full <- TeO.full %>% mutate( ## Recoding as Muslims in the rel_gen classification those who did not self-select as such but who had Muslim parents. 
    rel_gen = case_when(
      native==0 & rel_gen == 1 ~ 5,
      native==0 & rel_gen == 2 ~ 6,
      native==0 & rel_gen == 3 ~ 5,
      native==0 & rel_gen == 4 ~ 6,
      TRUE                      ~  as.numeric(rel_gen)
    )) 


TeO.full <- TeO.full %>% mutate(missing = 
	 ifelse(r_impvie=="NA",1,
	 ifelse(r_food=="NA",1,
	 ifelse(urban_cluster=="NA",1,
	 ifelse(situpro_cluster_work=="NA",1,
	 ifelse(sexee_1=="NA", 1,
	 ifelse(age=="NA", 1,
	 ifelse(f_dip=="NA", 1,
     ifelse(h_income=="NA", 1, 
	 ifelse(sub_hardship_b_1=="NA", 1,
	 ifelse(imm_rel_group=="NA", 1,
	 ifelse(d_perception_b_1=="NA", 1, 
	 ifelse(xp_interpersonal_disc_1=="NA", 1,
	 ifelse(r_impedu_b_1=="NA", 1,
	 ifelse(trans_ties_1=="NA", 1,
	 ifelse(t_d_neigh_maghreb_1=="NA", 1,
	 ifelse(t_d_neigh_ss_africa_1 =="NA", 1,0)))))))))))))))))

TeO.full.2 <- TeO.full %>% filter(missing==0 & native==0) # add filter for "native==0" to get Muslim respondents only. 

TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "sexee")  # add dummies for sexee
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "situpro") # add dummies for situpro
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "generation2") # add dummies for generation2
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "regional_origin2") # add dummies for regional_origin2
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "sub_hardship_b")
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "d_perception_b")
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "trans_ties")
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "t_d_neigh_maghreb")
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "t_d_neigh_ss_africa")
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "imm_rel_group")
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "r_impedu_b")
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "xp_interpersonal_disc")
TeO.full.2  <- fastDummies::dummy_cols(TeO.full.2, select_columns = "rel_gen")

TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("sexee")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("situpro")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("generation2")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("regional_origin2")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("sub_hardship_b")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("d_perception_b")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("r_impedu")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("trans_ties")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("t_d_neigh_maghreb")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("t_d_neigh_ss_africa")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("imm_rel_group")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("r_impedu_b")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("xp_d_instit")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("xp_interpersonal_disc")), as.double)
TeO.full.2 <- TeO.full.2 %>% mutate_at(vars(matches("rel_gen")), as.double)

clustering_var_means_list <- NULL

for (d in 1:draws) {

variable_draw <- paste0("A",d)
variable_draw <- eval(parse(text = paste0("TeO.full.2$",variable_draw)))

TeO.full.2 <- TeO.full.2 %>% mutate(assignment=variable_draw) # Here the name needs to be different, otherwise it gets confused 
														      # keeps data from the first iteration.

clustering_var_means <-  TeO.full.2 %>% group_by(assignment) %>% 
					  summarise(mean_impvie=mean(r_impvie, ), sd_impvie=sd(r_impvie),
					  		    mean_food=mean(r_food), sd_food=sd(r_food),
					 		 	mean_catholic=mean(imm_rel_group_3), sd_catholic=sd(imm_rel_group_3),
							   	mean_muslim=mean(imm_rel_group_2), sd_muslim=sd(imm_rel_group_2),
							   	mean_native=mean(imm_rel_group_1), sd_native=sd(imm_rel_group_1),
							   	mean_urban=mean(urban_cluster), sd_urban=sd(urban_cluster),
                                mean_situpro=mean(situpro_cluster_work), sd_situpro=sd(situpro_cluster_work),
							   	mean_sexee=mean(sexee), sd_sexee=sd(sexee), 
                                mean_age=mean(age), sd_age=sd(age), 
                                mean_f_dip=mean(f_dip_bin), sd_f_dip=sd(f_dip_bin), 
                                mean_h_income=mean(h_income), sd_h_income=sd(h_income), 
                                mean_sub_hardship_b_1=mean(sub_hardship_b_1), sd_sub_hardship_b_1=sd(sub_hardship_b_1),
                                mean_d_perception_b_1=mean(d_perception_b_1), sd_d_perception_b_1=sd(d_perception_b_1),
                                mean_xp_interpersonal_disc_1=mean(xp_interpersonal_disc_1), sd_xp_interpersonal_disc_1=sd(xp_interpersonal_disc_1),
                                mean_r_impedu_b_1=mean(r_impedu_b_1), sd_r_impedu_b_1=sd(r_impedu_b_1),	
                                mean_trans_ties_1=mean(trans_ties_1), sd_trans_ties_1=sd(trans_ties_1),
                                mean_t_d_neigh_maghreb_1=mean(t_d_neigh_maghreb_1), sd_t_d_neigh_maghreb_1=sd(t_d_neigh_maghreb_1),
                                mean_t_d_neigh_ss_africa_1=mean(t_d_neigh_ss_africa_1), sd_t_d_neigh_ss_africa_1=sd(t_d_neigh_ss_africa_1),
                             #   mean_generation2_2=mean(generation2_2, na.rm=TRUE), sd_generation2_2=sd(generation2_2, na.rm=TRUE) # only if necessary, no coding to make it clean below. Notice the "na.rm=TRUE"

)

cluster_count <- count(TeO.full.2, assignment)										
																			  
clustering_var_means <- clustering_var_means %>% mutate(iteration=d) 

clustering_var_means <- clustering_var_means %>% add_column(cluster_count$n)

clustering_var_means_list[[d]] <- clustering_var_means # This adds each iteration to the empty list.

}

clustering_var_means_list <- dplyr::bind_rows(clustering_var_means_list)

names(clustering_var_means_list)[39] <- "count" # this needs adjusting if more columns are added above. 

clustering_var_grand_means.2 <- clustering_var_means_list %>% group_by(assignment) %>%
	  summarise(grand_mean_impvie=mean(mean_impvie), cluster_error_impvie=sd(mean_impvie), grand_sd_impvie=mean(sd_impvie),
	  		    grand_mean_food=mean(mean_food), cluster_error_food=sd(mean_food), grand_sd_food=mean(sd_food),
	 		 	grand_mean_catholic=mean(mean_catholic), cluster_error_catholic=sd(mean_catholic), grand_sd_catholic=mean(sd_catholic),
			   	grand_mean_muslim=mean(mean_muslim), cluster_error_muslim=sd(mean_muslim), grand_sd_muslim=mean(sd_muslim),
			   	grand_mean_native=mean(mean_native), cluster_error_native=sd(mean_native), grand_sd_native=mean(sd_native),
			   	grand_mean_urban=mean(mean_urban), cluster_error_urban=sd(mean_urban), grand_sd_urban=mean(sd_urban),
				grand_mean_situpro=mean(mean_situpro), cluster_error_situpro=sd(mean_situpro), grand_sd_situpro=mean(sd_situpro),
				grand_mean_sexee=mean(mean_sexee), cluster_error_sexee=sd(mean_sexee), grand_sd_sexee=mean(sd_sexee),
				grand_mean_age=mean(mean_age), cluster_error_age=sd(mean_age), grand_sd_age=mean(sd_age),
				grand_mean_f_dip=mean(mean_f_dip), cluster_error_f_dip=sd(mean_f_dip), grand_sd_f_dip=mean(sd_f_dip),
				grand_mean_h_income=mean(mean_h_income), cluster_error_h_income=sd(mean_h_income), grand_sd_h_income=mean(sd_h_income),
				grand_mean_sub_hardship_b_1=mean(mean_sub_hardship_b_1), cluster_error_sub_hardship_b_1=sd(mean_sub_hardship_b_1), grand_sd_sub_hardship_b_1=mean(sd_sub_hardship_b_1),
				grand_mean_d_perception_b_1=mean(mean_d_perception_b_1), cluster_error_d_perception_b_1=sd(mean_d_perception_b_1), grand_sd_d_perception_b_1=mean(sd_d_perception_b_1),
				grand_mean_xp_interpersonal_disc_1=mean(mean_xp_interpersonal_disc_1), cluster_error_xp_interpersonal_disc_1=sd(mean_xp_interpersonal_disc_1), grand_sd_xp_interpersonal_disc_1=mean(sd_xp_interpersonal_disc_1),
				grand_mean_r_impedu_b_1=mean(mean_r_impedu_b_1), cluster_error_r_impedu_b_1=sd(mean_r_impedu_b_1), grand_sd_r_impedu_b_1=mean(sd_r_impedu_b_1),
				grand_mean_trans_ties_1=mean(mean_trans_ties_1), cluster_error_trans_ties_1=sd(mean_trans_ties_1), grand_sd_trans_ties_1=mean(sd_trans_ties_1),
				grand_mean_t_d_neigh_maghreb_1=mean(mean_t_d_neigh_maghreb_1), cluster_error_t_d_neigh_maghreb_1=sd(mean_t_d_neigh_maghreb_1), grand_sd_t_d_neigh_maghreb_1=mean(sd_t_d_neigh_maghreb_1),
				grand_mean_t_d_neigh_ss_africa_1=mean(mean_t_d_neigh_ss_africa_1), cluster_error_t_d_neigh_ss_africa_1=sd(mean_t_d_neigh_ss_africa_1), grand_sd_t_d_neigh_ss_africa_1=mean(sd_t_d_neigh_maghreb_1),
				grand_count=mean(count), cluster_error_count=sd(count),
				grand_mean_generation2_2=mean(mean_generation2_2)

)

clustering_var_grand_means <- transpose(clustering_var_grand_means.2)

colnames(clustering_var_grand_means) <- rownames(clustering_var_grand_means.2)
rownames(clustering_var_grand_means) <- colnames(clustering_var_grand_means.2)

clustering_var_grand_means$'1'  <- as.numeric(as.character(clustering_var_grand_means$'1' )) ## not ideal and not elegant but oh well.
clustering_var_grand_means$'2'  <- as.numeric(as.character(clustering_var_grand_means$'2' ))
clustering_var_grand_means$'3'  <- as.numeric(as.character(clustering_var_grand_means$'3' ))
clustering_var_grand_means$'4'  <- as.numeric(as.character(clustering_var_grand_means$'4' ))

clustering_var_grand_means <- clustering_var_grand_means <- cbind(variable = rownames(clustering_var_grand_means), clustering_var_grand_means) #creates a column with variable names


clustering_error <- clustering_var_grand_means %>% filter(str_detect(variable, 'cluster_error')) # separating clustering error for a new table
clustering_error$variable <- as.character(clustering_error$variable) # renaming  the varibles appropriately

							clustering_error[1,1] <- "Empirical SD: Subjective religiosity"
							clustering_error[2,1] <- "Empirical SD: Following dietary constraints"
							clustering_error[3,1] <- "Empirical SD: Prop. Catholic immigrants"
							clustering_error[4,1] <- "Empirical SD: Prop. Muslim immigrants"
							clustering_error[5,1] <- "Empirical SD: Prop. natives"
							clustering_error[6,1] <- "Empirical SD: Prop. living in 100k+ city"
							clustering_error[7,1] <- "Empirical SD: Prop. working a job"
							clustering_error[8,1] <- "Empirical SD: Prop. female"
							clustering_error[9,1] <- "Empirical SD: Age"
							clustering_error[10,1] <- "Empirical SD: Prop. with HS degree or more"
							clustering_error[11,1] <- "Empirical SD: Household income in k€"
							clustering_error[12,1] <- "Empirical SD: Prop. reporting subjective hardship"
							clustering_error[13,1] <- "Empirical SD: Prop. perceiving high level of discrimination"
							clustering_error[14,1] <- "Empirical SD: Prop. reporting discrimination within last 5 years"
							clustering_error[15,1] <- "Empirical SD: Prop. reporting high importance of religion in parental education"
							clustering_error[16,1] <- "Empirical SD: Prop. reporting transnational ties"
							clustering_error[17,1] <- "Empirical SD: Prop. living in top decile for % of North African immigrants"
							clustering_error[18,1] <- "Empirical SD: Prop. living in top decile for % of Sub-S African immigrants"
							clustering_error[19,1] <- "Empirical SD: Count"



clustering_var_grand_means <- clustering_var_grand_means %>% filter(str_detect(variable, 'cluster_error') == FALSE)
clustering_var_grand_means$variable <- as.character(clustering_var_grand_means$variable)

							clustering_var_grand_means[1,1] <- "Cluster assignment" # renaming  the varibles appropriately
							clustering_var_grand_means[2,1] <- "Grand mean: Subjective religiosity"
							clustering_var_grand_means[3,1] <- "Grand SD: Subjective religiosity"
							clustering_var_grand_means[4,1] <- "Grand mean: Following dietary constraints"
							clustering_var_grand_means[5,1] <- "Grand SD: Following dietary constraints"
							clustering_var_grand_means[6,1] <- "Grand mean: Prop. Catholic immigrants"
							clustering_var_grand_means[7,1] <- "Grand SD: Prop. Catholic immigrants"
							clustering_var_grand_means[8,1] <- "Grand Mean: Prop. Muslim immigrants"
							clustering_var_grand_means[9,1] <- "Grand SD: Prop. Muslim immigrants"
							clustering_var_grand_means[10,1] <- "Grand mean:  Prop. natives"
							clustering_var_grand_means[11,1] <- "Grand SD:  Prop. natives"
							clustering_var_grand_means[12,1] <- "Grand mean: Prop. living in 100k+ city"
							clustering_var_grand_means[13,1] <- "Grand SD: Prop. living in 100k+ city"
							clustering_var_grand_means[14,1] <- "Grand mean: Prop. working a job"
							clustering_var_grand_means[15,1] <- "Grand SD: Prop. working a job"
							clustering_var_grand_means[16,1] <- "Grand mean: Prop. female"
							clustering_var_grand_means[17,1] <- "Grand SD: Prop. female"
							clustering_var_grand_means[18,1] <- "Grand mean: Age"
							clustering_var_grand_means[19,1] <- "Grand SD: Age"
							clustering_var_grand_means[20,1] <- "Grand mean: Prop. with HS degree or mor"
							clustering_var_grand_means[21,1] <- "Grand SD: Prop. with HS degree or mor"
							clustering_var_grand_means[22,1] <- "Grand mean: Household income in k€"
							clustering_var_grand_means[23,1] <- "Grand SD: Household income in k€"
							clustering_var_grand_means[24,1] <- "Grand mean: Prop. reporting subjective hardship"
							clustering_var_grand_means[25,1] <- "Grand SD: Prop. reporting subjective hardship"
							clustering_var_grand_means[26,1] <- "Grand mean: perceiving high level of discrimination"
							clustering_var_grand_means[27,1] <- "Grand SD: perceiving high level of discrimination"
							clustering_var_grand_means[28,1] <- "Grand mean: reporting discrimination within last 5 years"
							clustering_var_grand_means[29,1] <- "Grand SD: reporting discrimination within last 5 years"
							clustering_var_grand_means[30,1] <- "Grand mean: reporting high importance of religion in parental education"
							clustering_var_grand_means[31,1] <- "Grand SD: reporting high importance of religion in parental education"
							clustering_var_grand_means[32,1] <- "Grand mean: Prop. reporting transnational ties"
							clustering_var_grand_means[33,1] <- "Grand SD: Prop. reporting transnational ties"
							clustering_var_grand_means[34,1] <- "Grand mean: Prop. living in top decile for % of North African immigrants"
							clustering_var_grand_means[35,1] <- "Grand SD: Prop. living in top decile for % of North African immigrants"
							clustering_var_grand_means[36,1] <- "Grand mean: Prop. living in top decile for % of Sub-S African immigrants"
							clustering_var_grand_means[37,1] <- "Grand SD: Prop. living in top decile for % of Sub-S African immigrants"
							clustering_var_grand_means[38,1] <- "Grand count"

clustering_var_grand_means[,-1] <- round(clustering_var_grand_means[,-1],2) ## Rounding
clustering_error[,-1] <- round(clustering_error[,-1],2)

### Producing pie charts

clustering_var_grand_means <- rbind(clustering_var_grand_means,clustering_error) # this is put empirical standard errors together with

###### Producing tables 

clustering_results_table <- clustering_var_grand_means %>% filter(str_detect(variable, 'Grand SD') == FALSE)  # 

kable(clustering_results_table[1:32, 1:5], "latex", booktabs = T, linesep = "", caption = "Descriptive statistics on the analytical sample for variables of interest") %>%
kable_styling(position = "center") 

kable(clustering_error[1:6, 1:5], "latex", booktabs = T, linesep = "", caption = "Average variation across 500 iterations with probabilistic assignment") %>%
kable_styling(position = "center") 


rm(TeO.full.2,clustering_var_means_list, clustering_var_means, clustering_var_grand_means.2)
