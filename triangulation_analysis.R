#
# Triangulation Analysis -------------------------------------------------------
#

# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
#summ(MVR_GI_model, digits = 4)



#for(i in 2:5){

#
# Set paths --------------------------------------------------------------------
#

setwd("C:/Users/lg531/OneDrive - University of Exeter/Studies/Application_Study/analysis_CPRD_Aurum/triangulation_analysis")

result_path <- "C:/Users/lg531/OneDrive - University of Exeter/Studies/Application_Study/analysis_CPRD_Aurum/triangulation_analysis/results"
data_path   <- "C:/Users/lg531/OneDrive - University of Exeter/Studies/Application_Study/analysis_CPRD_Aurum/data"

#
# Packages ---------------------------------------------------------------------
#

# install.packages("jtools")
library(jtools)

# install.packages("lme4")
library(lme4)

#
# Some further necessary information -------------------------------------------
#
variables_withNAs <- c("prehba1c", "preegfr", "prebmi", "preweight", "prealt", "ethnicity_cat", "deprivation", "smoking_cat")
dont_delete       <- c("result_path", "data_path", "year_type", "censoring_type", "population_type", "which_outcome", "outcome_variable_type", "variables_withNAs")

#
# Decision on analysis ---------------------------------------------------------
#

year_type       <- c("1year","3year")[2]
censoring_type  <- c("A","B")[1]
population_type <- c("study_population", "not_elderly", "elderly", "study_population_female", "study_population_male")[2]


#
# Genital infection models -----------------------------------------------------
# 

which_outcome         <- "GI"
outcome_variable_type <- "binary"

source("triangulation_method_summary.R")


#
# Osmotic Symptoms models ------------------------------------------------------
#


which_outcome         <- "OS"
outcome_variable_type <- "binary"

source("triangulation_method_summary.R")


#
# Micturition control models ---------------------------------------------------
#


which_outcome         <- "MC"
outcome_variable_type <- "binary"

source("triangulation_method_summary.R")


#
# Volume depletion models ------------------------------------------------------
#


which_outcome         <- "VD"
outcome_variable_type <- "binary"

source("triangulation_method_summary.R")


#
# Urinary frequency models -----------------------------------------------------
#


which_outcome         <- "UF"
outcome_variable_type <- "binary"

source("triangulation_method_summary.R")


#
# Falls models -----------------------------------------------------------------
#


which_outcome         <- "falls"
outcome_variable_type <- "binary"

source("triangulation_method_summary.R")


#
# Amputation models ------------------------------------------------------------
#


which_outcome         <- "amputation"
outcome_variable_type <- "binary"

source("triangulation_method_summary.R")


#
# DKA models -------------------------------------------------------------------
#


which_outcome         <- "dka"
outcome_variable_type <- "binary"

source("triangulation_method_summary.R")


#
# Lower limb fractures and falls (combined) models -----------------------------
#


which_outcome         <- "llffalls"
outcome_variable_type <- "binary"

source("triangulation_method_summary.R")



#
# HbA1c models -----------------------------------------------------------------
#


which_outcome         <- "HbA1c"
outcome_variable_type <- "continuous"

source("triangulation_method_summary.R")


#
# Weight models ----------------------------------------------------------------
#


which_outcome         <- "weight"
outcome_variable_type <- "continuous"

source("triangulation_method_summary.R")


#print(i)

#}




