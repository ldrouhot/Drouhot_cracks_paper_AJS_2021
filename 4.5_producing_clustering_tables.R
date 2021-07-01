TeO.full <- read.dta13(paste(path.data,"TeO_full_post_fuzzy_clustering_051920.dta",sep=""), convert.factors = FALSE) %>% as_tibble()

## Above, this is restoring the dataset obtained after the membership coefficients have been obtained. 

################################################# PRODUCING GRAND MEANS ACROSS CLUSTERING ASSIGNMENTS AND TABLE ############################################

TeO.full <- TeO.full %>% mutate( ## Recoding as Muslims in the rel_gen classification those who did not self-select as such but who had Muslim parents. 
    rel_gen = case_when(
      native==0 & rel_gen == 1 ~ 5,
      native==0 & rel_gen == 2 ~ 6,
      native==0 & rel_gen == 3 ~ 5,
      native==0 & rel_gen == 4 ~ 6,
      TRUE                      ~  as.numeric(rel_gen)
    )) 

TeO.full.2 <- TeO.full %>% filter(native==1) ## Adjust this and the title below for graphing either natives or non-natives
										     ## Age filter is considering we have such a large prop. of "socially dependent" in Muslim 2G.

clustering_var_means_list <- NULL


for (d in 1:draws) {


variable_draw <- paste0("A",d)
variable_draw <- eval(parse(text = paste0("TeO.full.2$",variable_draw)))


TeO.full.2 <- TeO.full.2 %>% mutate(assignment=variable_draw) # Here the name needs to be different, otherwise it gets confused 
														      # keeps data from the first iteration.


clustering_var_means <-  TeO.full.2 %>% group_by(assignment) %>% summarise(mean_sexee=mean(sexee), sd_sexee=sd(sexee), 
                                                                          	  mean_age=mean(age), sd_age=sd(age), 
                                                                              mean_f_dip=mean(f_dip_bin), sd_f_dip=sd(f_dip_bin), 
                                                                              mean_h_income=mean(h_income), sd_h_income=sd(h_income), 
                                                                              mean_urban=mean(urban_cluster), sd_urban=sd(urban_cluster),
                                                                              mean_situpro=mean(situpro_cluster_work), sd_situpro=sd(situpro_cluster_work),
                                                                              mean_weight=mean(poidsin), mean_religiositysum=mean(religiositysum), sd_religiositysum=sd(religiositysum)) 
																
																 
cluster_count <- count(TeO.full.2, assignment)										
																			  
clustering_var_means <- clustering_var_means %>% mutate(iteration=d) 

clustering_var_means <- clustering_var_means %>% add_column(cluster_count$n)

clustering_var_means_list[[d]] <- clustering_var_means # This adds each iteration to the empty list.

}

clustering_var_means_list <- dplyr::bind_rows(clustering_var_means_list)

names(clustering_var_means_list)[18] <- "count" # this needs adjusting if more columns are added above. 

clustering_var_grand_means.2 <- clustering_var_means_list %>% group_by(assignment) %>%
					  summarise(grand_mean_sexee=mean(mean_sexee), cluster_error_sexee=sd(mean_sexee), grand_sd_sexee=mean(sd_sexee),
								grand_mean_age=mean(mean_age), cluster_error_age=sd(mean_age), grand_sd_age=mean(sd_age),
								grand_mean_f_dip=mean(mean_f_dip), cluster_error_f_dip=sd(mean_f_dip), grand_sd_f_dip=mean(sd_f_dip),
								grand_mean_h_income=mean(mean_h_income), cluster_error_h_income=sd(mean_h_income), grand_sd_h_income=mean(sd_h_income),
								grand_mean_urban=mean(mean_urban), cluster_error_urban=sd(mean_urban), grand_sd_urban=mean(sd_urban),
								grand_mean_situpro=mean(mean_situpro), cluster_error_situpro=sd(mean_situpro), grand_sd_situpro=mean(sd_situpro),
								grand_count=mean(count), cluster_error_count=sd(count), grand_mean_weight=mean(mean_weight),
								grand_mean_religiosity_sum=mean(mean_religiositysum), cluster_error_religiositysum=sd(mean_religiositysum), grand_sd_religiositysum=mean(sd_religiositysum))

clustering_var_grand_means <- transpose(clustering_var_grand_means.2)

colnames(clustering_var_grand_means) <- rownames(clustering_var_grand_means.2)
rownames(clustering_var_grand_means) <- colnames(clustering_var_grand_means.2)

clustering_var_grand_means$'1'  <- as.numeric(as.character(clustering_var_grand_means$'1' )) ## not ideal and not elegant but oh well.
clustering_var_grand_means$'2'  <- as.numeric(as.character(clustering_var_grand_means$'2' ))
clustering_var_grand_means$'3'  <- as.numeric(as.character(clustering_var_grand_means$'3' ))
clustering_var_grand_means$'4'  <- as.numeric(as.character(clustering_var_grand_means$'4' ))

clustering_var_grand_means <- clustering_var_grand_means <- cbind(variable = rownames(clustering_var_grand_means), clustering_var_grand_means) #creates a column with variable names


clustering_error <- clustering_var_grand_means %>% filter(str_detect(variable, 'cluster_error')) # separating clustering error for a new table
clustering_error$variable <- as.character(clustering_error$variable) # renaming  the variables appropriately

							clustering_error[1,1] <- "Empirical SD: Prop. female"
							clustering_error[2,1] <- "Empirical SD: Mean age"
							clustering_error[3,1] <- "Empirical SD: Prop. with HS degree or more"
							clustering_error[4,1] <- "Empirical SD: Household income in k€"
							clustering_error[5,1] <- "Empirical SD: Prop. living in 100k+ city"
							clustering_error[6,1] <- "Empirical SD: Prop. working a job"
							clustering_error[7,1] <- "Empirical SD: Standard deviation for cluster population"
							clustering_error[8,1] <- "Empirical SD: Religiosity (additive)"


clustering_var_grand_means <- clustering_var_grand_means %>% filter(str_detect(variable, 'cluster_error') == FALSE)
clustering_var_grand_means$variable <- as.character(clustering_var_grand_means$variable)

							clustering_var_grand_means[1,1] <- "Cluster assignment" # renaming  the varibles appropriately
							clustering_var_grand_means[2,1] <- "Grand mean: Prop. female"
							clustering_var_grand_means[3,1] <- "Grand SD: Prop. female"
							clustering_var_grand_means[4,1] <- "Grand mean: Age"
							clustering_var_grand_means[5,1] <- "Grand SD: Age"
							clustering_var_grand_means[6,1] <- "Grand mean: prop. with HS degree or more"
							clustering_var_grand_means[7,1] <- "Grand SD: prop. with HS degree or more"
							clustering_var_grand_means[8,1] <- "Grand Mean: Household income in k€"
							clustering_var_grand_means[9,1] <- "Grand SD: Household income in k€"
							clustering_var_grand_means[10,1] <- "Grand mean: Prop. living in 100k+ city"
							clustering_var_grand_means[11,1] <- "Grand SD: Prop. living in 100k+ city"
							clustering_var_grand_means[12,1] <- "Grand mean: Prop. working a job"
							clustering_var_grand_means[13,1] <- "Grand SD: Prop. working a job"
							clustering_var_grand_means[14,1] <- "Grand count"
							clustering_var_grand_means[15,1] <- "Grand mean: Weight"
							clustering_var_grand_means[16,1] <- "Grand mean: Religiosity (additive)"
							clustering_var_grand_means[17,1] <- "Grand SD: Religiosity (additive)"

clustering_var_grand_means[,-1] <- round(clustering_var_grand_means[,-1],2) ## Rounding
clustering_error[,-1] <- round(clustering_error[,-1],2)

## Cluster overlap graph

n_assignment <- draws+11

assignments <- TeO.full.2[,12:n_assignment]

plot(assignments$A349, assignments$A135, xlab="Assignment #349", ylab="Assignment #135", asp=1)
dev.print(pdf, 'assignment_overlap.pdf')

### Producing pie charts

clustering_var_grand_means <- rbind(clustering_var_grand_means,clustering_error) # this is put empirical standard errors together with

cluster_count <- transpose(clustering_var_grand_means) %>% row_to_names(row_number = 1)

CustomPaletteTrans <- c( "#ff4d4d","#ffa64d","#5cd65c","#3385ff")

cluster_count$`Grand count` = as.numeric(cluster_count$`Grand count`)
cluster_count$`Cluster assignment` = as.numeric(cluster_count$`Cluster assignment`)
cluster_count$`Grand mean: Weight` = as.numeric(cluster_count$`Grand mean: Weight`)
cluster_count$`Grand mean: Religiosity (additive)` = as.numeric(cluster_count$`Grand mean: Religiosity (additive)`)
cluster_count$`Empirical SD: Religiosity (additive)` = as.numeric(cluster_count$`Empirical SD: Religiosity (additive)`)

cluster_count <- cluster_count %>% mutate(weighted_count=(`Grand count`*`Grand mean: Weight`))

cluster_count <- cluster_count %>% mutate(Cluster = ifelse(`Cluster assignment`==1,as.character("1: Working class"),
													     ifelse(`Cluster assignment`==2,as.character("2: Peripheral petite bourgeoisie"),
													     ifelse(`Cluster assignment`==3,as.character("3: Urban middle class+"), as.character("4: Socially dependent")))))	

cluster_membership_breakdown <- ggplot(cluster_count, aes(x="", y=weighted_count, fill=Cluster)) + geom_col() +
			ggtitle("")  +
		    geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + 
			scale_fill_manual(values=CustomPaletteTrans) +
			theme_void(base_size = 15) + geom_text(aes(label = paste(round(weighted_count / sum(weighted_count) * 100, 1), "%", "[",paste0(`Cluster assignment`),"]")),
            position = position_stack(vjust = 0.5)) +  theme(legend.title=element_text(size=16),legend.text=element_text(size=16)) +
            theme(plot.title = element_text(hjust = 0.5))

 print(cluster_membership_breakdown)

dev.print(pdf, 'cluster_membership_breakdown_1G.pdf')

cluster_count <- cluster_count %>% mutate(ll_rel=`Grand mean: Religiosity (additive)`-1.96*(`Empirical SD: Religiosity (additive)`),ul_rel=`Grand mean: Religiosity (additive)`+1.96*(`Empirical SD: Religiosity (additive)`))

graph.native.rel <- cluster_count %>% 
	ggplot(aes(x=Cluster, y=`Grand mean: Religiosity (additive)`, group=Cluster , fill=Cluster )) +
	geom_bar(stat="identity", width=.70) +  geom_errorbar(aes(x=Cluster, ymin=ll_rel, ymax=ul_rel), width=0.4, colour="black", alpha=0.7, size=1) +
	labs(x= "", y = "Religiosity (cumulative), 0-8 scale") + scale_fill_manual(values=CustomPaletteTrans, guide=FALSE) +
	 theme_grey(base_size=15)

print(graph.native.rel)

dev.print(pdf, 'rel_by_cluster_native.pdf')

###### Producing tables 

clustering_results_table <- clustering_var_grand_means %>% filter(str_detect(variable, 'Grand SD') == FALSE)  # 

kable(clustering_results_table[1:18, 1:5], "latex", booktabs = T, linesep = "", caption = "Social structure of native French population") %>%
kable_styling(position = "center") 

kable(clustering_error[1:6, 1:5], "latex", booktabs = T, linesep = "", caption = "Average variation across 500 iterations with probabilistic assignment") %>%
kable_styling(position = "center") 



rm(TeO.full.2,clustering_var_means_list, clustering_var_means, clustering_var_grand_means.2)
