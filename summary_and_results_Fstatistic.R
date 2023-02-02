#
# Get F-Statistic results ------------------------------------------------------
#


#
# Decision on analysis ---------------------------------------------------------
#

year_type        <- c("1year","3year")[2]                                       # change year according to outcome follow up (sensitivity analysis)
censoring_type   <- c("A","B")[1]                                               # change according to censoring type (sensitivity analysis)

results_populations <- c("study_population", "not_elderly", "elderly")
results_models      <- c("IV_prevpatient", "IV_Ertefaie", "IV_Ertefaie_rirs")
IV_variable         <- c("IV_prevpatientSGLT2", "IV_ePP", "IV_ePP_rirs")
results_outcomes    <- c("GI", "OS", "AX", "HbA1c", "weightchange", "weight")

#
# Paths ------------------------------------------------------------------------
#

path <- "C:/Users/lg531/OneDrive - University of Exeter/Studies/Application_Study/analysis_CPRD_Aurum/triangulation_analysis/results/"

#
# Build empty results table ----------------------------------------------------
#


rownumber   <- length(results_models)*length(results_outcomes)
colnumber   <- 5

result_data           <- as.data.frame(matrix(NA, nrow = rownumber, ncol = colnumber))
colnames(result_data) <- c("IV_method", "outcome", "study_population", "not_elderly", "elderly")
result_data$IV_method <- rep(results_models, times = length(results_outcomes))
result_data$outcome   <- rep(results_outcomes, each = length(results_models))


for(o in 1:length(results_outcomes)){
  
  for(m in 1:length(results_models)){
    
    for(p in 1:length(results_populations)){
      
      results <- get(load(paste0(path, results_models[m],"_", results_outcomes[o], "_model1_",year_type,"_c", censoring_type, "_", results_populations[p], ".Rdata" )))
      F_stats <- summary(results)$coef[IV_variable[m],"z value"]^2
      
      result_data[result_data$IV_method == results_models[m] & result_data$outcome == results_outcomes[o] ,(2+p)] <- F_stats
      
      rm(results)
      rm(F_stats)
    }
  }
}


result_data[, c(3:5)] <- round(result_data[, c(3:5)], 4)

