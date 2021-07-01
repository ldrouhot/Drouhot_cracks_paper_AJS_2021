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
	
TeO.full <- TeO.full %>% filter(generation2!=1)												   	  


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
TeO.full <- TeO.full %>% mutate_at(vars(matches("r_impedu_b")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("xp_d_instit")), as.double)
TeO.full <- TeO.full %>% mutate_at(vars(matches("xp_interpersonal_disc")), as.double)



TeO.full <- TeO.full %>% mutate(imm_rel_group = factor(imm_rel_group)) ## change this in model specification, no more dummies. Keep dummies in other models 
																	   ## we want to graph specific effects. 


## Omission of dummies in the regression model specification determines the baseline category.

## Models

# Subset by cluster

surplus.models.combined <- NULL

# Below, draws is the number of times clustering assignments are drawn, while "variable_draw" is the resulting clustering assignment. 

for (d in 1:draws) {

variable_draw <- paste0("A",d)
variable_draw <- eval(parse(text = paste0("TeO.full$",variable_draw)))

TeO.full <- TeO.full %>% mutate(cluster_assignment=variable_draw)

surplus.models.bycluster <- NULL


for (c in 1:n_clusters) {

	subsample_TeO.full <- TeO.full %>% filter(cluster_assignment==c)
	

# A tidy object with the coefficients should be created for each model, with a left column containing each cluster, and another left column
# containing each iteration. Then each coefficient can be averaged across iteration, and presented across clusters. So first a "draws" column
# then a "variable_draw" column.


	# Model 99: Model with controls and all variables by cluster as well as interactions


	model_99 <- lm(r_impvie ~  imm_rel_group +

	sexee_1 +
	age +
	age_sq +
	f_dip +

	h_income + sub_hardship_b_1 + 

	d_perception_b_1 + xp_interpersonal_disc_1 + 

	r_impedu_b_1 + 

	trans_ties_1 +

	t_d_neigh_maghreb_1 + t_d_neigh_ss_africa_1  +

	imm_rel_group:h_income + # interaction terms for Muslims
	imm_rel_group:sub_hardship_b_1 +
	imm_rel_group:d_perception_b_1 +
	imm_rel_group:xp_interpersonal_disc_1 +
	imm_rel_group:r_impedu_b_1 +
	imm_rel_group:trans_ties_1 +
	imm_rel_group:t_d_neigh_maghreb_1 +
	imm_rel_group:t_d_neigh_ss_africa_1, data=subsample_TeO.full, weights=poidsin)

	summary(model_99)

	beta  <- summary(model_99)$coefficients[2,1]
	sigma <- summary(model_99)$coefficients[2,2]
	beta.stoch <- rnorm(1,beta,sigma)*-1

	low_religiosity_grid <- subsample_TeO.full %>% data_grid(cluster_assignment=c, ## First a grid for low predicted religiosity, setting key variables
		
		imm_rel_group,															## to values decreasing religiosity (e.g. high income, weak perception of disc, etc)
																			   ## The grid uses modal values by default, we use mean for continuous values here.
		sexee_1=mean(sexee_1,na.rm=TRUE),
		age=mean(age,na.rm=TRUE),
		age_sq=mean(age_sq,na.rm=TRUE),
		f_dip=mean(f_dip,na.rm=TRUE),

		h_income=quantile(h_income, 0.75),
		sub_hardship_b_1=mean(sub_hardship_b_1, na.rm=TRUE),

		d_perception_b_1=1,
		xp_interpersonal_disc_1=1,


		r_impedu_b_1=1,

		trans_ties_1=1,

		t_d_neigh_maghreb_1=1, # the other dummies for high presence of SS African immigrants is set to modal values by default, which in this case is 0. 
		
		.model=model_99)

	mean_native_rel <- subsample_TeO.full %>% filter(imm_rel_group==1) %>% summarise(mean_native_rel=mean(r_impvie, na.rm = TRUE)) # generating mean native religiosity by iteration and cluster

	surplus.model_99 <- low_religiosity_grid %>% add_predictions(model_99) %>%
	group_by(cluster_assignment, imm_rel_group) %>%
	mutate(iteration = d, model = "Complete model", Low = first(pred)) %>%
	dplyr::select(iteration, cluster_assignment, imm_rel_group, model,Low) 

	surplus.model_99 <- surplus.model_99 %>% mutate(mean_native_rel_in_cl=as.numeric(mean_native_rel))


	high_religiosity_grid <- subsample_TeO.full %>% data_grid(cluster_assignment=c, ## First a grid for low predicted religiosity, setting key variables
		
		imm_rel_group,													     	## to values decreasing religiosity (e.g. high income, weak perception of disc, etc)
																			   ## The grid uses modal values by default, we use mean for continuous values here.
		sexee_1=mean(sexee_1,na.rm=TRUE),
		age=mean(age,na.rm=TRUE),
		age_sq=mean(age_sq,na.rm=TRUE),
		f_dip=mean(f_dip,na.rm=TRUE),
		
		h_income=quantile(h_income, 0.25),
		sub_hardship_b_1=mean(sub_hardship_b_1, na.rm=TRUE),

		d_perception_b_1=1,
		xp_interpersonal_disc_1=1,


		r_impedu_b_1=1,

		trans_ties_1=1,

		t_d_neigh_maghreb_1=1, # the other dummies for high presence of SS African immigrants is set to modal values by default, which in this case is 0. 
		
		.model=model_99)

	surplus.model_99_2 <- high_religiosity_grid %>% add_predictions(model_99) %>%
	group_by(cluster_assignment, imm_rel_group) %>%
	mutate(iteration = d, model = "Complete model", High = first(pred)) %>%
	dplyr::select(iteration, cluster_assignment, imm_rel_group, model,High)


	#surplus.model_99 <- surplus.model_99 %>% add_column(mean_native_rel)

	# Put together model's high and low predicted values

	surplus.models <- rbind(surplus.model_99)
	surplus.models_2 <- rbind(surplus.model_99_2)
	surplus.models <- surplus.models %>% add_column(surplus.models_2$High) 
	colnames(surplus.models)[7] <- "High"



	surplus.models.bycluster <- rbind(surplus.models.bycluster,surplus.models)
	assign("results_by_cluster",surplus.models.bycluster)

}

rm(surplus.models_2)
rm(surplus.model_99)
rm(surplus.model_99_2)
rm(mean_native_rel)

surplus.models.combined <-  rbind(surplus.models.combined,results_by_cluster)

assign("results_full",surplus.models.combined)

}


#################################################################################
################################# GRAPHING ######################################
#################################################################################	

surplus.models.combined <- gather(surplus.models.combined, key="profile", value="pred", c(5,7)) # turning different predictions column into one

surplus.models.combined.profiles <- surplus.models.combined %>% group_by(cluster_assignment,imm_rel_group, profile) %>% 
	summarise(mean_pred=mean(pred), sd_pred=sd(pred), ll_pred=(mean_pred-1.96*sd_pred), ul_pred=(mean_pred+1.96*sd_pred), native_ref_mean=mean(mean_native_rel_in_cl))

muslim.profiles <- surplus.models.combined.profiles %>% filter(imm_rel_group==2) 

clusters <- c(
                    '1' = "1: Working class",
                    '2' = "2: Peripheral petite bourgeoisie",
                    '3' = "3: Urban middle class+",
                    '4' = "4: Socially dependent"
                    )

profile.graph.r_impvie <- muslim.profiles %>% 
ggplot(aes(x=factor(profile), group=factor(profile), y=mean_pred, ymin=ll_pred, ymax=ul_pred)) + coord_cartesian(ylim=c(0,2)) +
facet_grid( ~ cluster_assignment, labeller = as_labeller(clusters)) +
labs(title="", x= "", y = "Predicted religiosity: importance of religion in respondent's life (0-2 scale)", caption = "N.B: Horizontal line measures average predicted religiosity for natives") + geom_hline(data=muslim.profiles, aes(yintercept=native_ref_mean), color="red") + 
geom_pointrange() + theme(text = element_text(size=15)) 


print(profile.graph.r_impvie) # 


dev.print(pdf,'profile.graph.r_impvie_2G.pdf')

#################################################################################
################################# CLEANING UP ###################################
#################################################################################											    


rm(list = ls(pattern = "results_iter")) 
rm(list = ls(pattern = "all_coefs_cluster")) 
rm(list = ls(pattern = "results_cluster")) 
rm(results_by_cluster)

#a <- results_full %>% group_by(cluster_assignment,model) %>% summarise(surp = mean(surplus), surp.s = mean(surplus_stoch))

beep()

