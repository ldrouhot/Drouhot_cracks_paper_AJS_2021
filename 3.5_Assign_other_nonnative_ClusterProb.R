

TeO <- TeO %>% mutate(   ## Recoding secularized children of Christian immigrants
    relego1 = case_when(
      relpere1 == 10 & relego1 == 1 & religiositysum==0 ~ 10,
      relmere1 == 10 & relego1 == 1 & religiositysum==0 ~ 10,
      relmere1 != relpere1 & relpere1==10 & relego1 == 1 & religiositysum==0 ~ 10,

      TRUE ~  as.numeric(relego1)
    )) 

TeO <- TeO %>% mutate( ## Recoding secularized children of Catholic immigrants
    relego1 = case_when(
      relpere1 == 11 & relego1 == 1 & religiositysum==0 ~ 11,
      relmere1 == 11 & relego1 == 1 & religiositysum==0 ~ 11,
      relmere1 != relpere1 & relpere1==11 & relego1 == 1 & religiositysum==0 ~ 11,
   TRUE ~ as.numeric(relego1)
   ))

TeO <- TeO %>% mutate( ## Recoding secularized children of Protestant & Orthodox immigrants
    relego1 = case_when(
      relpere1 == 12 & relego1 == 1 & religiositysum==0 ~ 12,
      relmere1 == 12 & relego1 == 1 & religiositysum==0 ~ 12,
      relmere1 != relpere1 & relpere1==12 & relego1 == 1 & religiositysum==0 ~ 12,
   TRUE ~ as.numeric(relego1)
   ))



nonnative.sample.cluster.vars <- TeO %>% filter(rel_gen2<9) %>% 
  mutate(age_bin=as.numeric(age>median.age.native, na.rm = TRUE), f_dip_bin=as.numeric(f_dip>median.f_dip.native, na.rm = TRUE), 
         h_income_bin=as.numeric(h_income>median.h_income.native, na.rm = TRUE)) %>%
         dplyr::select(sexee, age_bin, f_dip_bin, h_income_bin, urban_cluster, situpro_cluster_work,religiositysum,ident) %>%
        drop_na() %>%
        dplyr::select(-religiositysum, -ident)

## Above we take the nonnative sample. Below we take the centers for the 4 fuzzy clusters.

cc <- my.fuzzy.cmeans$centers

dm <- sapply(seq_len(nrow(nonnative.sample.cluster.vars)),
             function(i) apply(cc, 1, function(v) sqrt(sum((nonnative.sample.cluster.vars[i, ]-v)^2))))


## Above, we calculate the distance for each nonnative respondent to the centers of the 4 clusters, using Euclidean distance. 

## Now below, compute membership values. Note the 1.3 fuzzier below.

m <- fuzzifier
probs.cluster.nonnative <- t(apply(dm, 2,
              function(x) {
                tmp <- 1/((x/sum(x))^(2/(m-1)))  # formula above
                tmp/sum(tmp)  # normalization
              }))

# replace NaNs by 1

is.nan.data.frame <- function(x)
do.call(cbind, lapply(x, is.nan))

probs.cluster.nonnative[is.nan(probs.cluster.nonnative)] <- 1


rm(nonnative.sample.cluster.vars) # Remove nonnative dataset for clustering.

############## Below, assign cluster membership probabilistically to each individual in nonnative sample. 


myTeO.nonnative <- TeO %>% filter(rel_gen2<9) %>% 
  mutate(age_bin=as.numeric(age>median.age.native, na.rm = TRUE), f_dip_bin=as.numeric(f_dip>median.f_dip.native, na.rm = TRUE), 
         h_income_bin=as.numeric(h_income>median.h_income.native, na.rm = TRUE)) %>%
         dplyr::select(sexee, age_bin, f_dip_bin, h_income_bin, urban_cluster, situpro_cluster_work,religiositysum, ident) %>%
        drop_na() 


ncols <- ncol(myTeO.nonnative)

myTeO.nonnative <- cbind(myTeO.nonnative,Hmisc::rMultinom(probs.cluster.nonnative,draws))

setnames(myTeO.nonnative, old = seq(ncols+1,ncols+draws,1), new = paste0("A",seq(1:draws)))


myTeO.nonnative <- myTeO.nonnative %>% mutate(native=-1)

# This helps avoid the shortcomings associated with a closest hard clustering assignment, which results in the same problems of determinisitc
# assignment associated with k-means. For a truly probabilistic assignment, we cannot simply take the cluster with the highest membership
# coefficient the c-means algorithm comes up with (i.e non-random assignment that does not account for uncertainty, e.g. a membership coefficeint of 0.55 will result
# in the same assignment likelihood as a .9 membership coefficients). Using membership coefficients does not account for any fuziness.
# To do so, we employ a multinomial draw. The "RMultinom" function is a random draw based on the probabilities given by the membership coefficient.
# The "1" indicate there is going to be only one draw. 
