
# =======================================================================================================================================
# ======================================================= CLUSTERING ===========================================================
# =======================================================================================================================================

# Clean data for clustering

myTeO <- TeO %>% filter(rel_gen==7) %>% 
  dplyr::select(age, f_dip, h_income, urban_cluster, situpro_cluster_work, religiositysum, ident) %>% #dplyr::select needed vars
  as_tibble() %>% #just a trick
  drop_na() # drops all observations with NAs

myTeO <- cbind(scale(myTeO[,1:5]), myTeO[,6:7]) # standardize only a subset

median.age.native <- TeO %>% filter(rel_gen==7) %>% with(median(age, na.rm = TRUE))
median.f_dip.native <- TeO %>% filter (rel_gen==7) %>% with(median(f_dip, na.rm = TRUE))
median.h_income.native <- TeO %>% filter (rel_gen==7) %>% with(median(h_income, na.rm = TRUE))


myTeO <- TeO %>% filter(rel_gen==7) %>% # filter TeO
  mutate(age_bin=as.numeric(age>median.age.native, na.rm = TRUE), f_dip_bin=as.numeric(f_dip>median.f_dip.native, na.rm = TRUE), 
         h_income_bin=as.numeric(h_income>median.h_income.native, na.rm = TRUE)) %>%
         dplyr::select(sexee, age_bin, f_dip_bin, h_income_bin, urban_cluster, situpro_cluster_work, religiositysum, ident) %>%
        drop_na()


# Run 3 different clustering methods: hard k-means, fanny and cmeans

my.fuzzy.fanny <- myTeO %>% dplyr::select(-religiositysum,-ident) %>% fanny(.,k=4,memb.exp = 1.3, maxit=1000, metric="manhattan") # Creates fuzzy fanny clusters

my.hard.kmeans <- myTeO %>% dplyr::select(-religiositysum,-ident) %>% kmeans(.,4) # Creates hard clusters

my.fuzzy.cmeans <- myTeO %>% dplyr::select(-religiositysum,-ident) %>% cmeans (., centers=4, iter.max=100, 
                                                                         verbose=TRUE, dist="manhattan",
                                                                         method="cmeans", m=1.3, 
                                                                         rate.par = NULL) # Creates fuzzy cmeans clusters



myTeO$cluster_fanny  <- my.fuzzy.fanny$clustering # adds cluster membership to TeO
myTeO$cluster_kmeans <- my.hard.kmeans$cluster # adds cluster membership to TeO
myTeO$cluster_cmeans <- my.fuzzy.cmeans$cluster # adds cluster membership to TeO



myTeO %>% as_tibble()


# Create plots with average religiosity for each cluster according to each methods

plot.fanny <-  myTeO %>% group_by(cluster_fanny) %>%
    summarise(mean = mean(religiositysum), se=sd(religiositysum)/sqrt(n())) %>% 
    ggplot(aes(x=factor(cluster_fanny), y=mean, fill=factor(cluster_fanny) )) +
    geom_bar(stat = "summary", fun.y = "mean") +
    geom_errorbar(aes(ymin=mean-1.98*se, ymax=mean+1.98*se), width=.2,position=position_dodge(.9)) +
    coord_cartesian(ylim=c(1,3)) + labs(x="Subgroups", y="Average Religiosity")

plot.kmeans <-  myTeO %>% group_by(cluster_kmeans) %>%
    summarise(mean = mean(religiositysum), se=sd(religiositysum)/sqrt(n())) %>% 
    ggplot(aes(x=factor(cluster_kmeans), y=mean, fill=factor(cluster_kmeans) )) +
    geom_bar(stat = "summary", fun.y = "mean") +
    geom_errorbar(aes(ymin=mean-1.98*se, ymax=mean+1.98*se), width=.2,position=position_dodge(.9)) +
    coord_cartesian(ylim=c(1,3)) + labs(x="Subgroups", y="Average Religiosity")

plot.cmeans <-  myTeO %>% group_by(cluster_cmeans) %>%
    summarise(mean = mean(religiositysum), se=sd(religiositysum)/sqrt(n())) %>% 
    ggplot(aes(x=factor(cluster_cmeans), y=mean, fill=factor(cluster_cmeans) )) +
    geom_bar(stat = "summary", fun.y = "mean") +
    geom_errorbar(aes(ymin=mean-1.98*se, ymax=mean+1.98*se), width=.2,position=position_dodge(.9)) +
    coord_cartesian(ylim=c(1,3)) + labs(x="Subgroups", y="Average Religiosity")


#plot_grid(plot.fanny,plot.kmeans,plot.cmeans, ncol=1, labels=c("A","B","C"))


table(myTeO$cluster_cmeans,myTeO$cluster_fanny)
table(myTeO$cluster_kmeans,myTeO$cluster_fanny)
table(myTeO$cluster_kmeans,myTeO$cluster_cmeans)

write.dta(myTeO, "natives_post_clustering.dta")