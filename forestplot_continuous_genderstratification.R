#
# Forest plot (continuous outcomes, gender stratification) -------------------------
#

# https://rdrr.io/cran/forestplot/man/forestplot.html

#
# Packages ---------------------------------------------------------------------
#

# install.packages("ggplot2")
library(ggplot2)

# install.packages("Cairo")
library(Cairo)

#install.packages("forestplot")
library(forestplot)

#install.packages("dplyr")
library(dplyr)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("Gmisc")
library(Gmisc)

#install.packages("abind")
library(abind)


#
# Set paths --------------------------------------------------------------------
#

setwd("C:/Users/lg531/OneDrive - University of Exeter/Studies/Application_Study/analysis_CPRD_Aurum/triangulation_analysis")

result_path <- "C:/Users/lg531/OneDrive - University of Exeter/Studies/Application_Study/analysis_CPRD_Aurum/triangulation_analysis/results"


#
# Decision on analysis ---------------------------------------------------------
#

year_type        <- c("1year","3year")[2]                                       # change year according to outcome follow up (sensitivity analysis)
censoring_type   <- c("A","B")[1]                                               # change according to censoring type (sensitivity analysis)

results_populations <- c("study_population_female", "study_population_male")

results_models      <- c("MVR", "PSM", "IV_Ertefaie")
treatment_name      <- c("treatment","treatment", "X_hat_IV_Ertefaie")

results_outcomes    <- c("HbA1c", "weight") 

#
# Build empty results table ----------------------------------------------------
#


rownumber   <- length(results_models)*length(results_populations)*length(results_outcomes)
colnumber   <- 6

result_data           <- as.data.frame(matrix(NA, nrow = rownumber, ncol = colnumber))
colnames(result_data) <- c("outcome","cohort", "causal_estimate", "lower", "upper", "methods")
result_data$cohort    <- rep(rep(results_populations, each = length(results_models)), times = length(results_outcomes))
result_data$methods   <- rep(rep(results_models, times = length(results_populations)), times = length(results_outcomes))
result_data$outcome   <- rep(results_outcomes, each = length(results_models)*length(results_populations))

#
# Load the result data in results table ----------------------------------------
#


for(o in 1:length(results_outcomes)){
  
  for(p in 1:length(results_populations)){
    
    for(m in 1:length(results_models)){
      
      subset_results <- get(load(paste0(result_path, "/",results_models[m], "_", results_outcomes[o], "_modelsummary_", year_type, "_c", censoring_type, "_",results_populations[p],".Rdata" )))
      
      if(results_outcomes[o] == "HbA1c" | results_outcomes[o] == "weightchange" | results_outcomes[o] == "weight"){
        
        result_data$causal_estimate[result_data$cohort == results_populations[p] & result_data$methods == results_models[m] & result_data$outcome == results_outcomes[o]] <- (subset_results$coeftable[treatment_name[m], "Est."])
        result_data$lower[result_data$cohort == results_populations[p] & result_data$methods == results_models[m] & result_data$outcome == results_outcomes[o]]           <- (subset_results$coeftable[treatment_name[m], "2.5%"])
        result_data$upper[result_data$cohort == results_populations[p] & result_data$methods == results_models[m] & result_data$outcome == results_outcomes[o]]           <- (subset_results$coeftable[treatment_name[m], "97.5%"])
        
        
      }else{
        
        result_data$causal_estimate[result_data$cohort == results_populations[p] & result_data$methods == results_models[m] & result_data$outcome == results_outcomes[o]] <- exp(subset_results$coeftable[treatment_name[m], "Est."])
        result_data$lower[result_data$cohort == results_populations[p] & result_data$methods == results_models[m] & result_data$outcome == results_outcomes[o]]           <- exp(subset_results$coeftable[treatment_name[m], "2.5%"])
        result_data$upper[result_data$cohort == results_populations[p] & result_data$methods == results_models[m] & result_data$outcome == results_outcomes[o]]           <- exp(subset_results$coeftable[treatment_name[m], "97.5%"])
        
        
      }
      
      
      rm(subset_results)
    }
    
  }
  
}

result_data$cohort[result_data$cohort == "study_population_female"] <- "female"
result_data$cohort[result_data$cohort == "study_population_male"]   <- "male"



#
# Create structured results dataset --------------------------------------------
#

## data for plot in correct format (this works fine) ----
empty_row                    <- rep(NA, times = length(results_models))
rownames_outcomes_population <- c("HbA1c", "female", "male", "weight", "female", "male")
colnames_outcomes_population <- c("RR", "L", "U")

MVR <- rbind(empty_row,
       as.numeric(result_data[result_data$outcome == "HbA1c" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),  
       as.numeric(result_data[result_data$outcome == "HbA1c" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]),  
       empty_row, 
       as.numeric(result_data[result_data$outcome == "weight" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),     
       as.numeric(result_data[result_data$outcome == "weight" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]) 
       )     
rownames(MVR) <- rownames_outcomes_population
colnames(MVR) <- colnames_outcomes_population


PSM <- rbind(empty_row,
             as.numeric(result_data[result_data$outcome == "HbA1c" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),   
             as.numeric(result_data[result_data$outcome == "HbA1c" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5]),  
             empty_row, 
             as.numeric(result_data[result_data$outcome == "weight" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),   
             as.numeric(result_data[result_data$outcome == "weight" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5])
)     
rownames(PSM) <- rownames_outcomes_population
colnames(PSM) <- colnames_outcomes_population



IV <- rbind(empty_row,
             as.numeric(result_data[result_data$outcome == "HbA1c" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),   
             as.numeric(result_data[result_data$outcome == "HbA1c" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5]), 
             empty_row, 
             as.numeric(result_data[result_data$outcome == "weight" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]), 
             as.numeric(result_data[result_data$outcome == "weight" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5])
)     
rownames(IV) <- rownames_outcomes_population
colnames(IV) <- colnames_outcomes_population



plot_data <- abind(MVR, PSM, IV, along = 3)




#
# Forests plots showing all methods --------------------------------------------
#


## create the forest plot (all methods, all outcomes) --------------------------



Cairo(file = "forestplot_full_genderstratification_continuous.png", 
      type = "png",
      units = "in", 
      width = 10, 
      height = 8, 
      pointsize = 12, 
      dpi = 72)


forestplot(plot_data,
           xlog = F,
           legend=c("MVR", "PSM", "IV"),
           ci.vertices = TRUE,
           ci.vertices.height = 0.15, 
           boxsize = 0.2,
           zero = 1,
           txt_gp = fpTxtGp(cex = 1, xlab = gpar(cex = 1), ticks = gpar(cex = 1)),
           col = fpColors(box = c("blue", "darkred", "green"), lines = c("black", "black", "black")),
           fn.ci_norm = c("fpDrawCircleCI",  "fpDrawNormalCI","fpDrawDiamondCI"), 
           xlab = "relative risk (%)",
           new_page = TRUE)


dev.off()



#
# Forest plot only IV + additional incidence rate information ------------------
#


## Data preparation ----


load("C:/Users/lg531/OneDrive - University of Exeter/Studies/Application_Study/analysis_CPRD_Aurum/data/study_cohort_3year_wIV.Rdata")

data <- study_cohort_wIV
rm(study_cohort_wIV)




outcome_names           <- c("HbA1c", "     female", "     male", "weight", "     female", "     male")

mean                    <- as.numeric(plot_data[, 1 , 3])
lower                   <- as.numeric(plot_data[, 2 , 3])
upper                   <- as.numeric(plot_data[, 3 , 3])

text_col                <- paste(round(mean, 3), rep(" (", length(mean)),round(lower, 3),", ",round(upper,3),rep(")", length(mean)), sep = "")
text_col[which(text_col == "NA (NA, NA)")] <- ""
data_plot_paper         <- data.frame(cbind(outcome_names,  mean, lower, upper, text_col))

data_plot_paper$mean  <- as.numeric(data_plot_paper$mean)
data_plot_paper$lower <- as.numeric(data_plot_paper$lower)
data_plot_paper$upper <- as.numeric(data_plot_paper$upper)



## make the plot ----

Cairo(file = "forestplot_IV_genderstratification_continuous.png", 
      type = "png",
      units = "in", 
      width = 20,#10 
      height = 8, 
      pointsize = 12, 
      dpi = 72)

data_plot_paper|>
forestplot(labeltext = c(outcome_names, text_col),
           xlog = F,
           ci.vertices = TRUE,
           ci.vertices.height = 0.15, 
           boxsize = 0.2,
           zero = 1,
           col = fpColors(box = c("blue"), lines = c("black")),
           fn.ci_norm = c("fpDrawCircleCI"), 
           xlab = "estimate",
           txt_gp = fpTxtGp(cex = 1, xlab = gpar(cex = 1), ticks = gpar(cex = 1)),
           lty.ci = c(1),
           new_page = TRUE)|>
  fp_add_header(outcome_names = "adverse effects", text_col = "estimate (95% CI)") |>
  fp_add_lines(h_2 = gpar(lty = 1), h_5 = gpar(lty = 2))|> 
  fp_decorate_graph(graph.pos = 2)


dev.off()





