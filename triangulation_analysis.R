#
# Triangulation Analysis -------------------------------------------------------
#

# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
#summ(MVR_GI_model, digits = 4)


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

dont_delete     <- c("result_path", "data_path", "year_type", "censoring_type", "population_type", "which_outcome", "outcome_variable_type")

#
# Decision on analysis ---------------------------------------------------------
#

year_type       <- c("1year","3year")[2]
censoring_type  <- c("A","B")[1]
population_type <- c("study_population", "not_elderly", "elderly", "study_population_female", "study_population_male")[1]


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


