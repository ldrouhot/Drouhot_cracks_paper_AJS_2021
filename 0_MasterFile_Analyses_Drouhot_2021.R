
## May 2021

## LUCAS DROUHOT

#  MAX PLANCK INSTITUTE FOR THE STUDY OF RELIGIOUS AND ETHNIC DIVERSITY, GÖTTINGEN, GERMANY
#  DEPARTMENT OF SOCIOLOGY, UTRECHT UNIVERSITY (AS OF SEPT 1ST, 2021)

# This annotated program reproduced analyses from:
 
# Drouhot, Lucas G. 2021. "Cracks in the Melting Pot ? Religiosity & Assimilation Among the Diverse Muslim Population in France" American Journal of Sociology 126(4) 795-851.

# Original data source is the "Trajectoires et Origines" survey from 2008. More information on dataset and access at:
# http://bdq.quetelet.progedo.fr/fr/Details_d_une_enquete/1808

# Comments, criticisms and feedback on the Inductive Subgroup Comparison Approach welcome.

# Contact: drouhot@mmg.mpg.de 
# Website: lucasdrouhot.com


# =====================================================================================================================================
# ============================================================ PREAMBLE ===============================================================
# =====================================================================================================================================

# Clear Screen 

print("Hello")

cat("\014") # clean screen
rm(list = ls()) # remove every existing object


# Load Packages 

library("data.table")
library("arm")
library("arsenal")
library("beepr")
library("broom")
library("cluster")
library("clValid")
library("cowplot")
library("dotwhisker")
library("dplyr")
library("e1071")
library("factoextra")
library("fastDummies")
library("fclust")
library("foreign")
library("fpc")
library("ggplot2")
library("ggsci")
library("haven")
library("Hmisc")
library("janitor")
library("kableExtra")
library("LICORS")
library("magrittr")
library("modelr")
library("NbClust")
library("nnet")
library("plyr")
library("purrr")
library("readr")
library("readstata13")
library("stargazer")
library("tibble")
library("tidyr")
library("tidyverse")
library("xtable")


options(xtable.floating = FALSE)
options(xtable.timestamp = "")
options(scipen = 9999) 



# =====================================================================================================================================
# ========================================================== OPEN DATASET =============================================================
# =====================================================================================================================================

user <- c("") # input user name

path.analysis <- paste0("/Users/",user,"/") # input path to main work folder here
path.output   <- paste0("/Users/",user,"/") # input path to produced figures and tables here
path.data     <- paste0("/Users/",user,"/") # input path to data here


newseed1 <- 8789
set.seed(newseed1)
setwd(path.data)

## Note that for all analyses below I used a version of the TeO data cleaned and prepared in Stata.
## This version is called 


file.name <- paste(path.data,"file name here",sep="")
TeO    <- read.dta13(file.name, convert.factors = FALSE) %>% as_tibble()


setwd(path.analysis)

source("Functions.R")


# =====================================================================================================================================
# ====================================================== COMPARE CLUSTERING SOLUTIONS =================================================
# =====================================================================================================================================

# Description: Run 3 different clustering methods: hard k-means, fanny and cmeans

setwd(path.analysis)

source("1_ComparisonClusterSols.R")

n_clusters <- 4 # the number of groups into which natives and immigrants are partitioned.

draws <- 500 # this sets the number of iterations "d": that is, the number of random draw from the each observation's p distribution of membership coefficients.

start <- Sys.time()


	# =====================================================================================================================================
	# ============================================= PROBABILISTICALLY ASSIGN EACH PERSON TO CLUSTER =======================================
	# =====================================================================================================================================

	setwd(path.analysis)

	source("2_Assign_Native_ClusterProb.R")

	# =====================================================================================================================================
	# ============================= PROBABILISTICALLY ASSIGN EACH MUSLIM RESPONDENT TO A CLUSTER OF REFERENCE =============================
	# =====================================================================================================================================

	setwd(path.analysis)

	source("3_Assign_Muslim_ClusterProb.R")

	# =====================================================================================================================================
	# ============================ PROBABILISTICALLY ASSIGN EACH OTHER NON-NATIVE RESPONDENT TO A CLUSTER OF REFERENCE ====================
	# =====================================================================================================================================

	setwd(path.analysis)

	source("3.5_Assign_other_nonnative_ClusterProb.R")

	# =====================================================================================================================================
	# ================================================= COMBINE DATA MUSLIMS AND NATIVES ==================================================
	# =====================================================================================================================================

	setwd(path.analysis)

	source("4_binding_Natives_Muslims_together.R")

	# =====================================================================================================================================
	# ================================================= DECOMPOSING RELIGIOSITY SURPLUS =======++++++++====================================
	# =====================================================================================================================================
	
	setwd(path.analysis)

	source("5_decomposing_religiosity_surplus.R")

	# =====================================================================================================================================
	# ============================================== REGRESSIONS & GAP DECOMPOSITION ======================================================
	# =====================================================================================================================================

	setwd(path.analysis)

	source("6_regressions_simplified_large_models_other_migrant_groups_interactions_r_impvie.R")
	source("6_regressions_simplified_large_models_other_migrant_groups_interactions_r_food.R")		

	source("7_profile_analysis_r_impvie.R")
	source("7.1_profile_analysis_r_impvie_income.R")
	source("7.2_profile_analysis_r_impvie_income_disc.R")
	source("7.3_profile_analysis_r_impvie_lower_income_disc_low_soc")
	source("7.4_profile_analysis_r_impvie_lower_income_no_disc_low_soc_no_ties")

	source("7_profile_analysis_r_food.R")
	source("7.1_profile_analysis_r_food_income.R")
	source("7.2_profile_analysis_r_food_income_disc.R")
	source("7.3_profile_analysis_r_food_lower_income_no_disc_low_soc.R")
	source("7.4_profile_analysis_r_food_lower_income_no_disc_low_soc_no_ties.R")



end <- Sys.time()

time <- end - start; print(time)

beep()






