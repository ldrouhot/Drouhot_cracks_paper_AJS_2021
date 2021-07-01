



TeO <- TeO %>% mutate( # Recoding non-Muslim with no religion from Muslim parents as Muslims with 0 religiosity score.
    relego1 = case_when(
      relpere1 == 20 & relego1 == 1 & religiositysum==0 ~ 20,
      relmere1 == 20 & relego1 == 1 & religiositysum==0 ~ 20,
      relmere1 != relpere1 & relpere1==20 & relego1 == 1 & religiositysum==0 ~ 20,
      TRUE                      ~  as.numeric(relego1)
    )) 

muslim.sample.cluster.vars <- TeO %>% filter(relego1==20) %>% 
  mutate(age_bin=as.numeric(age>median.age.native, na.rm = TRUE), f_dip_bin=as.numeric(f_dip>median.f_dip.native, na.rm = TRUE), 
         h_income_bin=as.numeric(h_income>median.h_income.native, na.rm = TRUE)) %>%
         dplyr::select(sexee, age_bin, f_dip_bin, h_income_bin, urban_cluster, situpro_cluster_work,religiositysum,ident) %>%
        drop_na() %>%
        dplyr::select(-religiositysum, -ident)

## Above we take the Muslim sample. Below we take the centers for the 4 fuzzy clusters.

cc <- my.fuzzy.cmeans$centers

dm <- sapply(seq_len(nrow(muslim.sample.cluster.vars)),
             function(i) apply(cc, 1, function(v) sqrt(sum((muslim.sample.cluster.vars[i, ]-v)^2))))


## Above, we calculate the distance for each muslim respondent to the centers of the 4 clusters, using Euclidean distance. 

## Now below, compute membership values. Note the 1.3 fuzzier below.

m <- fuzzifier
probs.cluster.muslim <- t(apply(dm, 2,
              function(x) {
                tmp <- 1/((x/sum(x))^(2/(m-1)))  # formula above
                tmp/sum(tmp)  # normalization
              }))

# replace NaNs by 1

is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

probs.cluster.muslim[is.nan(probs.cluster.muslim)] <- 1


rm(muslim.sample.cluster.vars) # Remove muslim dataset for clustering.

############## Below, assign cluster membership probabilistically to each individual in Muslim sample. 


myTeO.muslim <- TeO %>% filter(relego1==20) %>% 
  mutate(age_bin=as.numeric(age>median.age.native, na.rm = TRUE), f_dip_bin=as.numeric(f_dip>median.f_dip.native, na.rm = TRUE), 
         h_income_bin=as.numeric(h_income>median.h_income.native, na.rm = TRUE)) %>%
         dplyr::select(sexee, age_bin, f_dip_bin, h_income_bin, urban_cluster, situpro_cluster_work,religiositysum, ident) %>%
        drop_na() 


ncols <- ncol(myTeO.muslim)

myTeO.muslim <- cbind(myTeO.muslim,Hmisc::rMultinom(probs.cluster.muslim,draws))

setnames(myTeO.muslim, old = seq(ncols+1,ncols+draws,1), new = paste0("A",seq(1:draws)))

myTeO.muslim <- myTeO.muslim %>% mutate(native=0)

# This helps avoid the shortcomings associated with a closest hard clustering assignment, which results in the same problems of determinisitc
# assignment associated with k-means. For a truly probabilistic assignment, we cannot simply take the cluster with the highest membership
# coefficient the c-means algorithm comes up with (i.e non-random assignment that does not account for uncertainty, e.g. a membership coefficeint of 0.55 will result
# in the same assignment likelihood as a .9 membership coefficients). Using membership coefficients does not account for any fuziness.
# To do so, we employ a multinomial draw. The "RMultinom" function is a random draw based on the probabilities given by the membership coefficient.
# The "1" indicate there is going to be only one draw. 
