TeO.full <- read.dta13(paste(path.data,"TeO_full_post_fuzzy_clustering_051920.dta",sep=""), convert.factors = FALSE) %>% as_tibble()

## Create the dependent variable - a linear addition of r_food and r_impvie
TeO.full <- TeO.full %>% mutate(s_rel=r_food+r_impvie) %>% filter(native==0 | native==1 | native==-1) %>% as.data.frame()

## Set reference category
#TeO.full <- TeO.full %>% mutate(native = factor(native))

# Create new variables for analysis
TeO.full <- TeO.full %>% mutate(generation2 = ifelse(native==0,generation,0), regional_origin2= ifelse(native==0,regional_origin,0))
TeO.full <- TeO.full %>% mutate(d_perception_b = ifelse(d_perception==2,d_perception,1)) %>% mutate(d_perception_b=(d_perception_b-1))

TeO.full <- TeO.full %>% mutate(imm_rel_group = ifelse(native==1,1,
												ifelse(relego1==20,2,
												ifelse(rel_gen2==5 | rel_gen2==6,3,
												ifelse(rel_gen2==3 | rel_gen2==4 | rel_gen2==7 | rel_gen2==8,4,
												ifelse(rel_gen2==1 | rel_gen2==2,5,0))))))


TeO.full <- TeO.full %>% mutate(r_impedu_b=ifelse(r_impedu==1 | r_impedu==2 | r_impedu==3,0,1))

TeO.full <- TeO.full %>% mutate(xp_interpersonal_disc= # Specific discrimination measure. If disc in last 5 years due to: skin color
												   	  ifelse(d_pqdisc_e==1,1,0)) # origins 
												    # religion
												   	  
TeO.full <- TeO.full %>% mutate(generation=case_when(rel_gen2==3 ~ 1, # making one clear generational filter applying to protestants and christians too.
										   			 rel_gen2==4 ~ 2,
										   			 rel_gen2==7 ~ 1,
										   			 rel_gen2==8 ~ 2,
										   			 native==1 ~ 3,
										   		    	TRUE ~  as.numeric(generation)))

TeO.full <- TeO.full %>% filter(generation2!=1) # Filter for 1G/2G only. Title of graph below needs adapting.



# Dummy-fication for clear effect size graphs. 


TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "sexee")  # add dummies for sexee
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "situpro") # add dummies for situpro
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "generation2") # add dummies for generation2
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "regional_origin2") # add dummies for regional_origin2
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "sub_hardship_b")
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "d_perception_b")
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "trans_ties")
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "t_d_neigh_maghreb")
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "t_d_neigh_ss_africa")
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "imm_rel_group")
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "r_impedu_b")
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "xp_d_instit") # this is the institutional discrimination measures computed earlier in Stata.
TeO.full  <- fastDummies::dummy_cols(TeO.full, select_columns = "xp_interpersonal_disc")


TeO.full <- TeO.full %>% mutate_at(vars(matches("sexee")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("situpro")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("generation2")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("regional_origin2")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("sub_hardship_b")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("d_perception_b")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("r_impedu")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("trans_ties")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("t_d_neigh_maghreb")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("t_d_neigh_ss_africa")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("imm_rel_group")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("r_impedu_b")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("xp_d_instit")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("xp_interpersonal_disc")), as.double)

## Omission of dummies in the regression model specification determines the baseline category.

## Models

# Subset by cluster

surplus.models.combined <- NULL

model_fit_list_across_iterations <- NULL

# Below, draws is the number of times clustering assignments are drawn, while "variable_draw" is the resulting clustering assignment. 

for (d in 1:draws) {

variable_draw <- paste0("A",d)
variable_draw <- eval(parse(text = paste0("TeO.full$",variable_draw)))

TeO.full <- TeO.full %>% mutate(cluster_assignment=variable_draw)

surplus.models.bycluster <- NULL

model_fit_list <- NULL


for (c in 1:n_clusters) {

	subsample_TeO.full <- TeO.full %>% filter(cluster_assignment==c)
	

# A tidy object with the coefficients should be created for each model, with a left column containing each cluster, and another left column
# containing each iteration. Then each coefficient can be averaged across iteration, and presented across clusters. So first a "draws" column
# then a "variable_draw" column.


	# Model 99: Model with controls and all variables by cluster as well as interactions. 
	# All modeling uses OLS for comparability across groups, interpretability, simplicity,
	# and to allow for meaningful decomposition via predicted values in later analyses.


	model_99 <- glm(r_impvie ~ imm_rel_group_2 + imm_rel_group_3 + imm_rel_group_3 + imm_rel_group_4 + imm_rel_group_5 +

	sexee_1 +
	age +
	age_sq +
	f_dip +

	h_income + sub_hardship_b_1 + 

	d_perception_b_1 + xp_interpersonal_disc_1 + 

	r_impedu_b_1 + 

	trans_ties_1 +

	t_d_neigh_maghreb_1 + t_d_neigh_ss_africa_1  +

	imm_rel_group_2:h_income + # interaction terms for Muslims
	imm_rel_group_2:sub_hardship_b_1 +
	imm_rel_group_2:d_perception_b_1 +
	imm_rel_group_2:xp_interpersonal_disc_1 +

	imm_rel_group_2:r_impedu_b_1 +
	imm_rel_group_2:trans_ties_1 +
	imm_rel_group_2:t_d_neigh_maghreb_1 +
	imm_rel_group_2:t_d_neigh_ss_africa_1 +

	imm_rel_group_3:h_income + # interaction terms for Catholics
	imm_rel_group_3:sub_hardship_b_1 +
	imm_rel_group_3:d_perception_b_1 +
	imm_rel_group_3:xp_interpersonal_disc_1 +

	imm_rel_group_3:r_impedu_b_1 +
	imm_rel_group_3:trans_ties_1 +
	imm_rel_group_3:t_d_neigh_maghreb_1 +
	imm_rel_group_3:t_d_neigh_ss_africa_1 +


	imm_rel_group_4:h_income + # interaction terms for other Christians (self-identified "Christians" and "Protestants")
	imm_rel_group_4:sub_hardship_b_1 +
	imm_rel_group_4:d_perception_b_1 +
	imm_rel_group_4:xp_interpersonal_disc_1 +

	imm_rel_group_4:r_impedu_b_1 +
	imm_rel_group_4:trans_ties_1 +
	imm_rel_group_4:t_d_neigh_maghreb_1 +
	imm_rel_group_4:t_d_neigh_ss_africa_1 + 

	imm_rel_group_5:h_income + # interaction terms for non-religious (not meaningful but needed for model to be equivalent with those produced by)
	imm_rel_group_5:sub_hardship_b_1 + #use of factor variable "imm_rel_group" in profile analysis.
	imm_rel_group_5:d_perception_b_1 +
	imm_rel_group_5:xp_interpersonal_disc_1 +

	imm_rel_group_5:r_impedu_b_1 +
	imm_rel_group_5:trans_ties_1 +
	imm_rel_group_5:t_d_neigh_maghreb_1 +
	imm_rel_group_5:t_d_neigh_ss_africa_1, data=subsample_TeO.full, weights=poidsin)

	summary(model_99)

	model_fit <- summary(model_99)$adj.r.squared %>% as.data.frame()
	beta  <- summary(model_99)$coefficients[2,1]
	sigma <- summary(model_99)$coefficients[2,2]
	beta.stoch <- rnorm(1,beta,sigma)*-1

	grid <- subsample_TeO.full %>% data_grid(cluster_assignment=c, native,
	sexee_1=mean(sexee_1,na.rm=TRUE),
	age=mean(age,na.rm=TRUE),
	age_sq=mean(age_sq,na.rm=TRUE),
	f_dip=mean(f_dip,na.rm=TRUE),


	h_income=mean(h_income, na.rm=TRUE),
	sub_hardship_b_1=mean(sub_hardship_b_1, na.rm=TRUE),


	d_perception_b_1=mean(d_perception_b_1, na.rm=TRUE),


	r_impedu_b_1=mean(r_impedu_b_1, na.rm=TRUE),

	trans_ties_1=mean(trans_ties_1, na.rm=TRUE),

	t_d_neigh_maghreb_1=mean(t_d_neigh_maghreb_1, na.rm=TRUE),
	t_d_neigh_ss_africa_1=mean(t_d_neigh_ss_africa_1, na.rm=TRUE),

	.model=model_99)

	surplus.model_99 <- grid %>% add_predictions(model_99) %>%
	group_by(cluster_assignment) %>%
	mutate(iteration = d, model = "Complete model", surplus = first(pred) - last(pred), surplus_stoch = beta.stoch) %>%
	filter(native==0) %>% dplyr::select(iteration, cluster_assignment, pred,  model,surplus, surplus_stoch )

	# Put together all model estimates by cluster. Next step: creating another column for iteration number, and creating separate objects by cluster. 
	
	results <- list(model_99)
	names(results) <- paste0("", 1:1)
	all_coefs <- plyr::ldply(results, tidy, .id = "model")
	assign(paste("all_coefs_cluster", c, sep = ""), all_coefs)

	# Put together models' predicted surplus

	surplus.models <- rbind(surplus.model_99)
	surplus.models.bycluster <- rbind(surplus.models.bycluster,surplus.models)
	assign("results_by_cluster",surplus.models.bycluster)

	model_fit_list[[c]] <- model_fit

}

results_across_iterations <- lapply(ls(pattern = "all_coefs_cluster.*"), get)
names(results_across_iterations) <- paste0("C", 1:n_clusters)
assign(paste("results_iter_", d, sep = ""), results_across_iterations)

surplus.models.combined <-  rbind(surplus.models.combined,results_by_cluster)
assign("results_full",surplus.models.combined)

model_fit_list <- dplyr::bind_rows(model_fit_list)

model_fit_list_across_iterations[d] <- model_fit_list

}

model_fit_list_across_iterations <- do.call(rbind, model_fit_list_across_iterations)  # Model fit, each column is a cluster, each row is an iteration.
names(model_fit_list_across_iterations) <- c(fit.cluster1,fit.cluster2,fit.cluster3,fit.cluster4)


mean.adj.R2.impvie <- model_fit_list_across_iterations %>% as.data.frame() %>% summarise(mean.fit.cluster1=mean(V1),mean.fit.cluster2=mean(V2),
															  mean.fit.cluster3=mean(V3),mean.fit.cluster4=mean(V4))


aggregate_list_of_results <- lapply(ls(pattern = "results_iter_.*"), get) # This a list of iterations, each containing a list of cluster, each containing the 8 models.
names(aggregate_list_of_results) <- paste0("", 1:d)


	results_cluster_1 <- lapply(aggregate_list_of_results, `[[`, c('C1')) ## note the [[ which allows to bypass the C1 layer between "iteration" and the list of model elements.
	results_cluster_1 <- ldply(results_cluster_1, data.frame, .id = "iteration") ## this adds an "iteration" column to a dataframe based on the list. 
	results_cluster_2 <- lapply(aggregate_list_of_results, `[[`, c('C2')) 
	results_cluster_2 <- ldply(results_cluster_2, data.frame, .id = "iteration")
	results_cluster_3 <- lapply(aggregate_list_of_results, `[[`, c('C3')) 
	results_cluster_3 <- ldply(results_cluster_3, data.frame, .id = "iteration")
	results_cluster_4 <- lapply(aggregate_list_of_results, `[[`, c('C4')) 
	results_cluster_4 <- ldply(results_cluster_4, data.frame, .id = "iteration")
 
aggregate_list_of_results <- list(results_cluster_1, results_cluster_2, results_cluster_3, results_cluster_4)
names(aggregate_list_of_results) <- paste0("", 1:4)
aggregate_list_of_results <- ldply(aggregate_list_of_results, data.frame, .id = "cluster") # a bit cumbersome above, but this is the final table I wanted.

# Here, I am running a cross-cluster, pooled model for the last, right-hand column of the regression results. 

subsample_pooled_TeO.full <- TeO.full %>% filter(!is.na(cluster))

model_100 <- lm(formula = r_impvie ~  imm_rel_group_2 + imm_rel_group_3 + imm_rel_group_3 + imm_rel_group_4 + imm_rel_group_5 +
	sexee_1 +
	age +
	age_sq +
	f_dip +

	h_income + sub_hardship_b_1 + 

	d_perception_b_1 + xp_interpersonal_disc_1 + 

	r_impedu_b_1 + 

	trans_ties_1 +

	t_d_neigh_maghreb_1 + t_d_neigh_ss_africa_1 +


	imm_rel_group_2:h_income + # interaction terms for Muslims
	imm_rel_group_2:sub_hardship_b_1 +
	imm_rel_group_2:d_perception_b_1 +
	imm_rel_group_2:xp_interpersonal_disc_1 +

	imm_rel_group_2:r_impedu_b_1 +
	imm_rel_group_2:trans_ties_1 +
	imm_rel_group_2:t_d_neigh_maghreb_1 +
	imm_rel_group_2:t_d_neigh_ss_africa_1+

	imm_rel_group_3:h_income + # interaction terms for Catholics
	imm_rel_group_3:sub_hardship_b_1 +
	imm_rel_group_3:d_perception_b_1 +
	imm_rel_group_3:xp_interpersonal_disc_1 +

	imm_rel_group_3:r_impedu_b_1 +
	imm_rel_group_3:trans_ties_1 +
	imm_rel_group_3:t_d_neigh_maghreb_1 +
	imm_rel_group_3:t_d_neigh_ss_africa_1 +

	imm_rel_group_4:h_income + # interaction terms for other Christians (self-identified "Christians" and "Protestants")
	imm_rel_group_4:sub_hardship_b_1 +
	imm_rel_group_4:d_perception_b_1 +
	imm_rel_group_4:xp_interpersonal_disc_1 +

	imm_rel_group_4:r_impedu_b_1 +
	imm_rel_group_4:trans_ties_1 + 
	imm_rel_group_4:t_d_neigh_maghreb_1 +
	imm_rel_group_4:t_d_neigh_ss_africa_1 +

	imm_rel_group_5:h_income + # interaction terms for non-religious (not meaningful but needed for model to be equivalent with those produced by)
	imm_rel_group_5:sub_hardship_b_1 + #use of factor variable "imm_rel_group" in profile analysis.
	imm_rel_group_5:d_perception_b_1 +
	imm_rel_group_5:xp_interpersonal_disc_1 +

	imm_rel_group_5:r_impedu_b_1 +
	imm_rel_group_5:trans_ties_1 +
	imm_rel_group_5:t_d_neigh_maghreb_1 +
	imm_rel_group_5:t_d_neigh_ss_africa_1, data=subsample_pooled_TeO.full, weights=poidsin)

	summary(model_100)

	model_fit <- summary(model_100)$adj.r.squared %>% as.data.frame()
	beta  <- summary(model_100)$coefficients[2,1]
	sigma <- summary(model_100)$coefficients[2,2]
	beta.stoch <- rnorm(1,beta,sigma)*-1


mean.adj.R2.impvie <- cbind(mean.adj.R2.impvie, model_fit) 
colnames(mean.adj.R2.impvie)[5] = "pooled.model"	


#################################################################################
######################## PRODUCING EFFECT SIZE GRAPH ############################
#################################################################################	

## GRAPH 1: SURPLUS ACROSS CLUSTERS, AND POOLED MODEL

	
pooled_coefficients <- c(summary(model_100)$coefficients[,1]) # This is only the last cluster right now. Needs averaging.
pooled_standard_errors <-c(summary(model_100)$coefficients[,2])
pooled_significance <- c(summary(model_100)$coefficients[,4])

coefficients_pooled_model <- data.frame(pooled_coefficients,pooled_standard_errors,pooled_significance, stringsAsFactors = TRUE) ## Now the ide column needs to be shifted to the right. 
coefficients_pooled_model <- tibble::rownames_to_column(coefficients_pooled_model, "term") 
coefficients_pooled_model <- coefficients_pooled_model %>% mutate(cluster=0) # 0 means the pooled model here. 
coefficients_pooled_model <- coefficients_pooled_model %>% rename(mean_p_value=pooled_significance, mean_std.error=pooled_standard_errors, mean_coefficients=pooled_coefficients) # renaming the same way as below for binding.

coefficients_pooled_model$cluster <- factor(coefficients_pooled_model$cluster)
coefficients_pooled_model <- coefficients_pooled_model[c(5,4,1,2,3)]

# Now, need to make sure the loop runs smoothly and we can adjust the number of iteration. Next step: write a tidy code to create table averaging
# results across clusters, iterations, and models. Then change the models substantively. 

coefficients <- aggregate_list_of_results  %>% group_by(cluster, term) %>% summarise(mean_coefficients=mean(estimate), mean_std.error=sd(estimate), mean_p_value=mean(p.value)) ## this needs to be made wide but that proved too complicated with too many NA cells. Color coding for last model only for results inspections
coefficients <- bind_rows(coefficients, coefficients_pooled_model) 
coefficients <- coefficients %>% mutate_if(is.numeric, round, digits = 4) # trimming decimals for better readability

## GRAPH 2: COEFFICIENTS FOR KEY VARIABLES ACROSS CLUSTERS AND IN POOLED MODEL

# To do that I will use dwplot. First, we rename the coefficients below to order them properly for graphical display.

 
coefficients<- coefficients %>% mutate(term = replace(term, term == "imm_rel_group_2", "H0_imm_rel_group_2")) # re-labelling group net surplus
coefficients<- coefficients %>% mutate(term = replace(term, term == "imm_rel_group_3", "H0_imm_rel_group_3"))

coefficients<- coefficients %>% mutate(term = replace(term, term == "h_income", "H1_h_income")) #re-labelling main terms
coefficients<- coefficients %>%	mutate(term = replace(term, term == "sub_hardship_b_1", "H1_sub_hardship_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "d_perception_b_1", "H2_d_perception_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "xp_interpersonal_disc_1", "H2_xp_interpersonal_disc_1"))

coefficients<- coefficients %>%	mutate(term = replace(term, term == "r_impedu_b_1", "H3_r_impedu_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "trans_ties_1", "H4_trans_ties_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "t_d_neigh_maghreb_1", "H5_t_d_neigh_maghreb_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "t_d_neigh_ss_africa_1", "H5_t_d_neigh_ss_africa_1"))

coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_2:h_income", "inter_muslim_H1_h_income")) # re-labelling interaction terms: Muslims.
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_2:sub_hardship_b_1", "inter_muslim_H1_sub_hardship_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_2:d_perception_b_1", "inter_muslim_H2_d_perception_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_2:xp_interpersonal_disc_1", "inter_muslim_H2_xp_interpersonal_disc_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_2:r_impedu_b_1", "inter_muslim_H3_r_impedu_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_2:trans_ties_1", "inter_muslim_H4_trans_ties_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_2:t_d_neigh_maghreb_1", "inter_muslim_H5_t_d_neigh_maghreb_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_2:t_d_neigh_ss_africa_1", "inter_muslim_H5_t_d_neigh_ss_africa_1"))

coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_3:h_income", "inter_catholics_H1_h_income")) #re_labelling interactions terms: Catholics
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_3:sub_hardship_b_1", "inter_catholics_H1_sub_hardship_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_3:d_perception_b_1", "inter_catholics_H2_d_perception_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_3:xp_interpersonal_disc_1", "inter_catholics_H2_xp_interpersonal_disc_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_3:r_impedu_b_1", "inter_catholics_H3_r_impedu_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_3:trans_ties_1", "inter_catholics_H4_trans_ties_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_3:t_d_neigh_maghreb_1", "inter_catholics_H5_t_d_neigh_maghreb_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_3:t_d_neigh_ss_africa_1", "inter_catholics_H5_t_d_neigh_ss_africa_1"))

coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_4:h_income", "inter_christians_H1_h_income")) #re-labelling interactions terms: Other christians
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_4:sub_hardship_b_1", "inter_christians_H1_sub_hardship_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_4:d_perception_b_1", "inter_christians_H2_d_perception_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_4:xp_interpersonal_disc_1", "inter_christians_H2_xp_interpersonal_disc_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_4:r_impedu_b_1", "inter_christians_H3_r_impedu_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_4:trans_ties_1", "inter_christians_H4_trans_ties_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_4:t_d_neigh_maghreb_1", "inter_christians_H5_t_d_neigh_maghreb_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_4:t_d_neigh_ss_africa_1", "inter_christians_H5_t_d_neigh_ss_africa_1"))

coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_5:h_income", "inter_non_religious_H1_h_income")) #re-labelling interactions terms: Other christians
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_5:sub_hardship_b_1", "inter_non_religious_H1_sub_hardship_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_5:d_perception_b_1", "inter_non_religious_H2_d_perception_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_5:xp_interpersonal_disc_1", "inter_non_religious_H2_xp_interpersonal_disc_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_5:r_impedu_b_1", "inter_non_religious_H3_r_impedu_b_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_5:trans_ties_1", "inter_non_religious_H4_trans_ties_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_5:t_d_neigh_maghreb_1", "inter_non_religious_H5_t_d_neigh_maghreb_1"))
coefficients<- coefficients %>%	mutate(term = replace(term, term == "imm_rel_group_5:t_d_neigh_ss_africa_1", "inter_non_religious_H5_t_d_neigh_ss_africa_1"))


coefficients_subset <- coefficients %>% filter(str_detect(term, 'H0_imm_rel_group') | #this keeps the dummies of interest only
											   str_detect(term, 'h_income') | str_detect(term, 'sub_hardship') |
											   str_detect(term, 'd_perception') | str_detect(term, 'xp_interpersonal') | 
											   str_detect(term, 'r_impedu_b_1') |
											   str_detect(term, 'trans_ties_1') |
											   str_detect(term, 't_d_neigh')) # this includes both the ss_africa and maghreb terms


coefficients_subset <- coefficients_subset %>% filter(str_detect(term, 'inter_christians') == FALSE) #removing interaction terms containing Christians and non-religious because we are not

coefficients_subset <- coefficients_subset %>% filter(str_detect(term, 'inter_non_religious') == FALSE)	# ibid for non-religious																								 # plotting them after all.


# Plotting


names(coefficients_subset) <- c("model", "term", "estimate", "std.error", "p.value") # resetting names to accomodate dwplot syntax. 
																			  #Remember that "model" refers to cluster, and 99 is a general, cross-cluster model.
coefficients_subset$model <-  as.factor(coefficients_subset$model) 


var_brackets <- list(c("Net surplus", "Muslim immigrants (ref: natives)", "Catholic immigrants (ref: natives)"),
					 c("Main terms", "Income (k€)", "Presence of African immigrants in neighborhood (high)"),
					 c("Interaction terms: Muslims", "Muslim * income", "Muslim * African immigrants in neighborhood (high)"), # make sure this is consistent with the labels in relabeling above otherwise the code bugs                     
 				     c("Interaction terms: Catholics", "Catholic * income", "Catholic * African immigrants in neighborhood (high)"))


CustomPalette <- c( "red","orange","#33CC00","blue","black")

{dwplot(coefficients_subset, dodge_size = 0.65, alpha=0.5,
	vline = geom_vline(xintercept = 0, colour = "grey40", linetype =5)) %>% 

   relabel_predictors(c( H0_imm_rel_group_2= "Muslim immigrants (ref: natives)",  # relabel predictors: main terms 
   						 H0_imm_rel_group_3= "Catholic immigrants (ref: natives)",

   						 H1_h_income = "Income (k€)",                      
                         H1_sub_hardship_b_1 = "Subjective hardship (high)",
                         H2_d_perception_b_1 = "General perception of discrimination (high)", 
                         H2_xp_interpersonal_disc_1 = "Reports interpersonal discrimination",
                         H3_r_impedu_b_1 = "Importance of religion in parental education (high)", 
                         H4_trans_ties_1 = "Reports transnational ties", 
                         H5_t_d_neigh_maghreb_1 = "Presence of Maghribi immigrants in neighborhood (high)",
                         H5_t_d_neigh_ss_africa_1="Presence of African immigrants in neighborhood (high)",

                         inter_muslim_H1_h_income="Muslim * income", # relabel predictors: interaction terms (Muslims)
                         inter_muslim_H1_sub_hardship_b_1="Muslim * subjective hardship (high)",
                         inter_muslim_H2_d_perception_b_1="Muslim * general perception of discrimination (high)",
                         inter_muslim_H2_xp_interpersonal_disc_1 = "Muslim * interpersonal discrimination",
                         inter_muslim_H3_r_impedu_b_1="Muslim * importance of religion in parental education (high)",
                         inter_muslim_H4_trans_ties_1="Muslim * transnational ties",
                         inter_muslim_H5_t_d_neigh_maghreb_1="Muslim * Maghribi immigrants in neighborhood (high)",
                         inter_muslim_H5_t_d_neigh_ss_africa_1="Muslim * African immigrants in neighborhood (high)",

                         inter_catholics_H1_h_income="Catholic * income", # relabel predictors: interaction terms (Catholics)
                         inter_catholics_H1_sub_hardship_b_1="Catholic * subjective hardship (high)",
                         inter_catholics_H2_d_perception_b_1="Catholic * general perception of discrimination (high)",
                         inter_catholics_H2_xp_interpersonal_disc_1 = "Catholic * interpersonal discrimination",
                         inter_catholics_H3_r_impedu_b_1="Catholic * importance of religion in parental education (high)",
                         inter_catholics_H4_trans_ties_1="Catholic * transnational ties",
                         inter_catholics_H5_t_d_neigh_maghreb_1="Catholic * Maghribi immigrants in neighborhood (high)",
                         inter_catholics_H5_t_d_neigh_ss_africa_1="Catholic * African immigrants in neighborhood (high)")) +

   ggtitle("Predicting subjective religiosity: importance of religion in respondent's life (0-2 scale) 2G only")  + theme_grey(base_size = 15) +
				   
				        theme(legend.title=element_text(size=16),legend.text=element_text(size=16)) + 

				        scale_color_manual(name="Subgroup of reference in\nnative population",
				     	values=CustomPalette,
				        breaks=c(0,4,3,2,1),
                   	    labels=c("All respondents", "Socially dependent","Urban middle class+","Peripheral petite bourgeoisie","Working class" ))} %>% add_brackets(var_brackets)

dev.print(pdf, 'full_model_r_impvie_2G_only')

#################################################################################
################################# CLEANING UP ###################################
#################################################################################											    

rm(coefficients_pooled_model)
rm(pooled_coefficients)
rm(pooled_significance)
rm(all_coefs) # this removes the unnamed object, last in the iteration (so always cluster 4)
rm(results)
rm(list = ls(pattern = "results_iter")) 
rm(list = ls(pattern = "all_coefs_cluster")) 
rm(list = ls(pattern = "results_cluster")) 
rm(results_across_iterations)
rm(results_by_cluster)

a <- results_full %>% group_by(cluster_assignment,model) %>% summarise(surp = mean(surplus), surp.s = mean(surplus_stoch))



