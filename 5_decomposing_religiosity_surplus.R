
# ======================================= SURPLUS NATIVE - NN MUSLIMS =============================

TeO.full <- read.dta13(paste(path.data,"TeO_full_post_fuzzy_clustering_051920.dta",sep=""), convert.factors = FALSE) %>% as_tibble()

TeO.full.graph <- TeO.full %>% filter(native!=-1 & generation2!=1)

surplus_estimates_list <- NULL

for (d in 1:draws) { ## TRYING TO FIGURE OUT HOW TO AVERAGE THE GRAPH OVER ITERATIONS

variable_draw <- paste0("A",d)
variable_draw <- eval(parse(text = paste0("TeO.full.graph$",variable_draw)))

# subset of native and non-native muslims are the two main groups in TeO.full.graph.

# religiositysum

model.religiositysum <- lm(religiositysum ~ factor(native)*factor(variable_draw), data=TeO.full.graph)

grid <- TeO.full.graph %>% data_grid(variable_draw, native, .model=model.religiositysum )

surplus.religiositysum <- grid %>% add_predictions(model.religiositysum) %>% group_by(variable_draw) %>%
    mutate(dimension="religiositysum",  surplus = first(pred) - last(pred)) %>%
    filter(native==0)

# r_food 

model.r_food <- lm(r_food ~ factor(native)*factor(variable_draw), data=TeO.full.graph)

grid <- TeO.full.graph %>% data_grid(variable_draw, native, .model=model.r_food)

surplus.r_food <- grid %>% add_predictions(model.r_food) %>% group_by(variable_draw) %>%
    mutate(dimension="r_food", surplus = first(pred) - last(pred)) %>%
    filter(native==0)

# r_impvie 

model.r_impvie <- lm(r_impvie ~ factor(native)*factor(variable_draw), data=TeO.full.graph)

grid <- TeO.full.graph %>% data_grid(variable_draw, native, .model=model.r_impvie)

surplus.r_impvie <- grid %>% add_predictions(model.r_impvie) %>% group_by(variable_draw) %>%
    mutate(dimension="r_impvie",surplus = first(pred) - last(pred)) %>%
    filter(native==0)

# r_signe 

model.r_signe <- lm(r_signe ~ factor(native)*factor(variable_draw), data=TeO.full.graph)

grid <- TeO.full.graph %>% data_grid(variable_draw, native, .model=model.r_signe)

surplus.r_signe <- grid %>% add_predictions(model.r_signe) %>% group_by(variable_draw) %>%
    mutate(dimension="r_signe",surplus = first(pred) - last(pred)) %>%
    filter(native==0)

 # r_attendance

model.r_attendance <- lm(r_attendance ~ factor(native)*factor(variable_draw), data=TeO.full.graph)

grid <- TeO.full.graph %>% data_grid(variable_draw, native, .model=model.r_attendance)

surplus.r_attendance <- grid %>% add_predictions(model.r_attendance) %>% group_by(variable_draw) %>%
    mutate(dimension="r_attendance", surplus = first(pred) - last(pred)) %>%
    filter(native==0)

# Put together

surplus_estimates <- bind_rows(surplus.religiositysum, surplus.r_food, surplus.r_impvie, surplus.r_signe, surplus.r_attendance) 
surplus_estimates <- surplus_estimates %>% mutate(iteration=d)

surplus_estimates_list[[d]] <- surplus_estimates # This adds each iteration to the empty list.

}

surplus_estimates_list <- dplyr::bind_rows(surplus_estimates_list)

rm(TeO.full.graph)

##### GRAPHING #####

dimensions_of_religiosity <- c(
                    `r_attendance` = "Religious attendance",
                    `r_food` = "Respect of dietary constraints",
                    `r_impvie` = "Subjective religiosity",
                    `r_signe` = "Wearing a religious sign",
                    `religiositysum` = "Religiosity (cumulative)"
                    )



plot.surplus.native.muslim <- surplus_estimates_list %>% group_by(dimension, variable_draw) %>% 
summarise(mean_surplus=mean(surplus), sd_surplus=sd(surplus), ll_surplus=mean_surplus-1.96*sd_surplus, ul_surplus=mean_surplus+1.96*sd_surplus) %>%
ggplot(aes(x=factor(variable_draw), y=mean_surplus, group=factor(variable_draw) , fill=factor(variable_draw) )) +
geom_bar(stat="identity") +  geom_errorbar(aes(x=factor(variable_draw), ymin=ll_surplus, ymax=ul_surplus), width=0.4, colour="orange", alpha=0.9, size=1.3) +
facet_grid( ~ dimension, labeller = as_labeller(dimensions_of_religiosity)) +
labs(x= "", y = "Religious differential with natives") +
scale_fill_discrete(name="Subgroup of reference in\nnative population",
                    breaks=c("1", "2", "3","4"),
                    labels=c("1: Working class", "2: Peripheral petite bourgeoisie", "3: Urban middle class+", "4: Socially dependent")) +
guides(fill=guide_legend(
                 keywidth=1,
                 keyheight=1,
                 default.unit="cm")) + theme(text = element_text(size=20)) 

print(plot.surplus.native.muslim)
dev.print(pdf, 'plot.surplus.native.muslim.pdf')
    
# Patrick Bateman says: "Impressive. Very nice. Noe let's see the same graph for cumulative religiosity across immigrant group by generation." 

religiosity_ratios_across_groups <- TeO.full %>% filter(rel_gen2!=1, rel_gen2!=2, rel_gen2!=11) %>% group_by(rel_gen2) %>% 
summarise(mean_religiositysum=mean(religiositysum, na.rm = TRUE))

native_mean_religiosity <- TeO.full %>% filter(native==1) %>% summarise(mean(religiositysum))
religiosity_ratios_across_groups <- religiosity_ratios_across_groups %>% mutate(ratio=mean_religiositysum/native_mean_religiosity$`mean(religiositysum)`)

religiosity_ratios_across_groups <- religiosity_ratios_across_groups %>% mutate(rel_gen2=rel_gen2-2)

palette_rel_gen_ratios <-c( "#A93226","#E74C3C","#5499C7","#5DADE2","#16A085","#1ABC9C","#F5B041", "#F4D03F")

plot.relratio.rel_gen2 <- religiosity_ratios_across_groups %>% filter(rel_gen2<7) %>%
ggplot(aes(x=factor(rel_gen2), y=ratio, group=factor(rel_gen2) , fill=factor(rel_gen2) )) +
geom_bar(stat="identity") + 
coord_cartesian(ylim=c(1,3)) + labs(x= "", y = "Immigrant : native religiosity ratio") + geom_hline(yintercept = 1, color="red") +
scale_fill_manual(name="Self-identified religious group, by generation",
                    breaks=c("1", "2", "3","4","5","6","7","8"),
                    values=palette_rel_gen_ratios,
                    labels=c("1: Christian 1G", "2: Christian 2G", "3: Catholic 1G", 
                        "4: Catholic 2G", "5: Protestant 1G", "6: Protestant 2G", 
                        "7: Muslim 1G", "8: Muslim 2G")) +
guides(fill=guide_legend(
                 keywidth=1,
                 keyheight=1,
                 default.unit="cm")) +  theme(text = element_text(size=20)) 

print(plot.relratio.rel_gen2)

dev.print(pdf, 'plot.relratio.rel_gen2.pdf')



############################################ OTHER PLOTS (NOT USED IN THE PAPER) ################################################

## Below is a bootstrapping procedure to calculate uncertainty around 
### the religiosity ratios. The rationale behind this is to account for sampling uncertainty in the context of ratios 
### which are more complicated than means for which straight standard errors could be calculated. Upper values and lower values for ratios are
### less straightforward. Theoretical distribution of a ratio?? etc..

## We do this because we are not interested in averaging across clusters at this point. Hence we take uncertainty
## into account through sampling and not through cluster assignment.


subdata <- TeO.full %>% select(native, rel_gen2, religiositysum) %>% filter(native==-1 | native==0 | native==1) 


n_rows <- nrow(subdata) ## this takes same number of observations as in original sample (some can be selected twice this is where the variations comes from.)
iter=1
boot_results <- NULL


while (iter < 2000) {

    boot_sample <- sample_n(subdata,n_rows, replace=TRUE)

    results <- boot_sample %>% group_by(rel_gen2) %>% summarise(mu = mean(religiositysum)) %>% na.omit() %>%
    mutate(ratio = mu/last(mu))

    boot_results <- bind_rows(boot_results,results)

    iter= iter+1
}


boot_results %>% filter(rel_gen2!=1, rel_gen2!=2, rel_gen2!=11) %>%
    group_by(rel_gen2) %>% summarise(estimate =mean(ratio), ll=quantile(ratio,p=0.025), ul=quantile(ratio,p=0.975)) %>%
    mutate(rel = case_when(rel_gen2==3 ~ "CH", rel_gen2==4 ~ "CH", rel_gen2==5 ~ "CA", rel_gen2==6 ~ "CA",
      rel_gen2==7 ~ "PR", rel_gen2==8 ~ "PR", rel_gen2==9 ~ "MU", rel_gen2==10 ~ "MU")) %>%
    ggplot(aes(x=factor(rel_gen2),y=estimate,ymin=ll,ymax=ul,colour=rel)) + geom_pointrange()


#############################################

    
# This looks nice but maybe a better way is to present a muslim:native religiosity ratio.

# First, we need to average means and sds across iterations.




comparative_religiosity_means <- TeO.full %>% filter(native==1 | native==0) %>% 
group_by(A3, native) %>% summarise(r_attendance=mean(r_attendance,na.rm = TRUE), r_food=mean(r_food,na.rm = TRUE), r_impvie=mean(r_impvie,na.rm = TRUE), r_signe=mean(r_signe,na.rm = TRUE), religiositysum=mean(religiositysum,na.rm = TRUE))

religiosity_ratios <- comparative_religiosity_means %>% group_by(A3) %>% 
summarise(r_attendance=r_attendance[native==0]/r_attendance[native==1], r_food=r_food[native==0]/r_food[native==1], r_impvie=r_impvie[native==0]/r_impvie[native==1], 
r_signe=r_signe[native==1]/r_signe[native==0], religiositysum=religiositysum[native==0]/religiositysum[native==1]) %>% 
gather('r_attendance', 'r_food', 'r_impvie', 'r_signe', 'religiositysum', key="dimension", value="ratio")

# Here the religiosity means need averaging across iterations, with error bars representing SD across iterations. 

plot.relratio.native.muslim <- religiosity_ratios %>% 
ggplot(aes(x=factor(A3), y=ratio, group=factor(A3) , fill=factor(A3) )) +
geom_bar(stat="identity") + coord_cartesian(ylim=c(0.5,10)) +
facet_grid( ~ dimension, labeller = as_labeller(dimensions_of_religiosity)) +
labs(x= "", y = "Muslim : native ratio") + geom_hline(yintercept = 1, color="red") +
scale_fill_discrete(name="Subgroup of reference",
                    breaks=c("1", "2", "3","4"),
                    labels=c("1: Socially dependent", "2: Working class", "3: Peripheral petite bourgeoisie", "4: Urban middle class+")) +
guides(fill=guide_legend(
                 keywidth=1,
                 keyheight=1,
                 default.unit="cm"))

print(plot.relratio.native.muslim)





