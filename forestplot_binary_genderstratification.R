#
# Forest plot (binary outcomes, gender stratification) -------------------------
#

# https://rdrr.io/cran/forestplot/man/forestplot.html

#
# Packages ---------------------------------------------------------------------
#

# install.packages("ggplot2")
#library(ggplot2)

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

results_outcomes    <- c("GI", "OS", "MC", "VD", "UF", "falls", "llffalls" ,"amputation", "dka")  # change outcomes for which plot is generated

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
rownames_outcomes_population <- c("genital infection", "female", "male", "osmotic symptoms", "female", "male", 
                                  "Micturition control", "female", "male", "Volume Depletion", "female", "male",
                                  "Urinary frequency", "female", "male", "falls", "female", "male", 
                                  "falls + lower limp fracture", "female", "male", "amputation", "female", "male", 
                                  "DKA", "female", "male")
colnames_outcomes_population <- c("RR", "L", "U")

MVR <- rbind(empty_row,
       as.numeric(result_data[result_data$outcome == "GI" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),     # GI MVR (est, lower, upper, not male)
       as.numeric(result_data[result_data$outcome == "GI" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]),     # GI MVR (est, lower, upper, male)
       empty_row, 
       as.numeric(result_data[result_data$outcome == "OS" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),     # OS MVR (est, lower, upper, not male)
       as.numeric(result_data[result_data$outcome == "OS" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]),     # OS MVR (est, lower, upper, male)
       empty_row,
       as.numeric(result_data[result_data$outcome == "MC" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),
       as.numeric(result_data[result_data$outcome == "MC" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]),
       empty_row,
       as.numeric(result_data[result_data$outcome == "VD" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),
       as.numeric(result_data[result_data$outcome == "VD" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]),
       empty_row,
       as.numeric(result_data[result_data$outcome == "UF" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),
       as.numeric(result_data[result_data$outcome == "UF" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]),
       empty_row,
       as.numeric(result_data[result_data$outcome == "falls" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),
       as.numeric(result_data[result_data$outcome == "falls" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]),
       empty_row,
       as.numeric(result_data[result_data$outcome == "llffalls" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),
       as.numeric(result_data[result_data$outcome == "llffalls" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]),
       empty_row,
       as.numeric(result_data[result_data$outcome == "amputation" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),
       as.numeric(result_data[result_data$outcome == "amputation" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5]),
       empty_row,
       as.numeric(result_data[result_data$outcome == "dka" & result_data$cohort == "female" & result_data$methods == "MVR" ,3:5]),
       as.numeric(result_data[result_data$outcome == "dka" & result_data$cohort == "male"     & result_data$methods == "MVR" ,3:5])
       )     
rownames(MVR) <- rownames_outcomes_population
colnames(MVR) <- colnames_outcomes_population


PSM <- rbind(empty_row,
             as.numeric(result_data[result_data$outcome == "GI" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),     # GI MVR (est, lower, upper, not male)
             as.numeric(result_data[result_data$outcome == "GI" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5]),     # GI MVR (est, lower, upper, male)
             empty_row, 
             as.numeric(result_data[result_data$outcome == "OS" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),     # OS MVR (est, lower, upper, not male)
             as.numeric(result_data[result_data$outcome == "OS" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5]),     # OS MVR (est, lower, upper, male)
             empty_row,
             as.numeric(result_data[result_data$outcome == "MC" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),
             as.numeric(result_data[result_data$outcome == "MC" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "VD" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),
             as.numeric(result_data[result_data$outcome == "VD" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "UF" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),
             as.numeric(result_data[result_data$outcome == "UF" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "falls" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),
             as.numeric(result_data[result_data$outcome == "falls" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "llffalls" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),
             as.numeric(result_data[result_data$outcome == "llffalls" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "amputation" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),
             as.numeric(result_data[result_data$outcome == "amputation" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "dka" & result_data$cohort == "female" & result_data$methods == "PSM" ,3:5]),
             as.numeric(result_data[result_data$outcome == "dka" & result_data$cohort == "male"     & result_data$methods == "PSM" ,3:5])
)     
rownames(PSM) <- rownames_outcomes_population
colnames(PSM) <- colnames_outcomes_population



IV <- rbind(empty_row,
             as.numeric(result_data[result_data$outcome == "GI" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),     # GI MVR (est, lower, upper, not male)
             as.numeric(result_data[result_data$outcome == "GI" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5]),     # GI MVR (est, lower, upper, male)
             empty_row, 
             as.numeric(result_data[result_data$outcome == "OS" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),     # OS MVR (est, lower, upper, not male)
             as.numeric(result_data[result_data$outcome == "OS" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5]),     # OS MVR (est, lower, upper, male)
             empty_row,
             as.numeric(result_data[result_data$outcome == "MC" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),
             as.numeric(result_data[result_data$outcome == "MC" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "VD" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),
             as.numeric(result_data[result_data$outcome == "VD" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "UF" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),
             as.numeric(result_data[result_data$outcome == "UF" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "falls" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),
             as.numeric(result_data[result_data$outcome == "falls" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "llffalls" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),
             as.numeric(result_data[result_data$outcome == "llffalls" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "amputation" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),
             as.numeric(result_data[result_data$outcome == "amputation" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5]),
             empty_row,
             as.numeric(result_data[result_data$outcome == "dka" & result_data$cohort == "female" & result_data$methods == "IV_Ertefaie" ,3:5]),
             as.numeric(result_data[result_data$outcome == "dka" & result_data$cohort == "male"     & result_data$methods == "IV_Ertefaie" ,3:5])
)     
rownames(IV) <- rownames_outcomes_population
colnames(IV) <- colnames_outcomes_population



plot_data <- abind(MVR, PSM, IV, along = 3)




#
# Forests plots showing all methods --------------------------------------------
#


## create the forest plot (all methods, all outcomes) --------------------------



Cairo(file = "forestplot_full_genderstratification_binary.png", 
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
           ci.vertices.height = 0.3, 
           boxsize = 0.4,
           zero = 1,
           txt_gp = fpTxtGp(cex = 1, xlab = gpar(cex = 1), ticks = gpar(cex = 1)),
           col = fpColors(box = c("blue", "darkred", "green"), lines = c("black", "black", "black")),
           fn.ci_norm = c("fpDrawCircleCI",  "fpDrawNormalCI","fpDrawDiamondCI"), 
           xlab = "relative risk (%)",
           new_page = TRUE)


dev.off()




## create the forest plot (all methods) A --------------------------------------

plot_data_sub <- plot_data[1:15, , ]

Cairo(file = "forestplot_A_genderstratification_binary.png", 
      type = "png",
      units = "in", 
      width = 10, 
      height = 8, 
      pointsize = 12, 
      dpi = 72)


forestplot(plot_data_sub,
           xlog = F,
           legend=c("MVR", "PSM", "IV"),
           ci.vertices = TRUE,
           ci.vertices.height = 0.2, 
           boxsize = 0.1,
           zero = 1,
           txt_gp = fpTxtGp(cex = 1, xlab = gpar(cex = 1), ticks = gpar(cex = 1)),
           col = fpColors(box = c("blue", "darkred", "green"), lines = c("black", "black", "black")),
           fn.ci_norm = c("fpDrawCircleCI",  "fpDrawNormalCI","fpDrawDiamondCI"), 
           xlab = "relative risk (%)",
           new_page = TRUE)


dev.off()


## create the forest plot (all methods) B -------------------------------------- 


plot_data_sub <- plot_data[16:27, , ]

Cairo(file = "forestplot_B_genderstratification_binary.png", 
      type = "png",
      units = "in", 
      width = 10, 
      height = 8, 
      pointsize = 12, 
      dpi = 72)


forestplot(plot_data_sub,
           xlog = F,
           legend=c("MVR", "PSM", "IV"),
           ci.vertices = TRUE,
           ci.vertices.height = 0.2, 
           boxsize = 0.1,
           zero = 1,
           txt_gp = fpTxtGp(cex = 1, xlab = gpar(cex = 1), ticks = gpar(cex = 1)),
           col = fpColors(box = c("blue", "darkred", "green"), lines = c("black", "black", "black")),
           fn.ci_norm = c("fpDrawCircleCI",  "fpDrawNormalCI","fpDrawDiamondCI"), 
           xlab = "relative risk (%)",
           new_page = TRUE)


dev.off()



## create the forest plot (all methods) C -------------------------------------- 

plot_data_sub <- plot_data[16:24, , ]

Cairo(file = "forestplot_C_genderstratification_binary.png", 
      type = "png",
      units = "in", 
      width = 10, 
      height = 8, 
      pointsize = 12, 
      dpi = 72)


forestplot(plot_data_sub,
           xlog = F,
           legend=c("MVR", "PSM", "IV"),
           ci.vertices = TRUE,
           ci.vertices.height = 0.2, 
           boxsize = 0.1,
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


outcomes_of_interest     <- c("outcome_gi_cA", "outcome_osmo_cA", "outcome_micturition_control_cA", "outcome_volume_depletion_cA", 
                              "outcome_urinary_frequency_cA", "outcome_falls_cA", "outcome_llffalls_cA", 
                              "outcome_amputation_cA", "outcome_dka_cA")

followuptime_of_interest <- c("followup_days_GI_cA", "followup_days_OS_cA", "followup_days_MC_cA",
                              "followup_days_VD_cA", "followup_days_UF_cA", "followup_days_falls_cA", "followup_days_llffalls_cA",
                              "followup_days_amputation_cA", "followup_days_dka_cA")

### additional information: number of events ----

n_event_SGLT2_female   <- colSums(data[data$drugclass == "SGLT2" & data$gender_chr == 1, ][ , outcomes_of_interest ])
n_event_SGLT2_male     <- colSums(data[data$drugclass == "SGLT2" & data$gender_chr == 0, ][ , outcomes_of_interest ])

n_event_DPP4_female   <- colSums(data[data$drugclass == "DPP4" & data$gender_chr == 1, ][ , outcomes_of_interest ])
n_event_DPP4_male     <- colSums(data[data$drugclass == "DPP4" & data$gender_chr == 0, ][ , outcomes_of_interest ])


n_event_SGLT2 <-   c(" ", as.numeric(n_event_SGLT2_female["outcome_gi_cA"]),                  as.numeric(n_event_SGLT2_male["outcome_gi_cA"]),
                     " ", as.numeric(n_event_SGLT2_female["outcome_osmo_cA"]),                as.numeric(n_event_SGLT2_male["outcome_osmo_cA"]),
                     " ", as.numeric(n_event_SGLT2_female["outcome_micturition_control_cA"]), as.numeric(n_event_SGLT2_male["outcome_micturition_control_cA"]),
                     " ", as.numeric(n_event_SGLT2_female["outcome_volume_depletion_cA"]),    as.numeric(n_event_SGLT2_male["outcome_volume_depletion_cA"]),  
                     " ", as.numeric(n_event_SGLT2_female["outcome_urinary_frequency_cA"]),   as.numeric(n_event_SGLT2_male["outcome_urinary_frequency_cA"]),
                     " ", as.numeric(n_event_SGLT2_female["outcome_falls_cA"]),               as.numeric(n_event_SGLT2_male["outcome_falls_cA"]),
                     " ", as.numeric(n_event_SGLT2_female["outcome_llffalls_cA"]),            as.numeric(n_event_SGLT2_male["outcome_llffalls_cA"]),
                     " ", as.numeric(n_event_SGLT2_female["outcome_amputation_cA"]),          as.numeric(n_event_SGLT2_male["outcome_amputation_cA"]),
                     " ", as.numeric(n_event_SGLT2_female["outcome_dka_cA"]),                 as.numeric(n_event_SGLT2_male["outcome_dka_cA"])
                    )  
                     
n_event_DPP4  <-   c(" ", as.numeric(n_event_DPP4_female["outcome_gi_cA"]),                  as.numeric(n_event_DPP4_male["outcome_gi_cA"]),
                     " ", as.numeric(n_event_DPP4_female["outcome_osmo_cA"]),                as.numeric(n_event_DPP4_male["outcome_osmo_cA"]),
                     " ", as.numeric(n_event_DPP4_female["outcome_micturition_control_cA"]), as.numeric(n_event_DPP4_male["outcome_micturition_control_cA"]),
                     " ", as.numeric(n_event_DPP4_female["outcome_volume_depletion_cA"]),    as.numeric(n_event_DPP4_male["outcome_volume_depletion_cA"]),  
                     " ", as.numeric(n_event_DPP4_female["outcome_urinary_frequency_cA"]),   as.numeric(n_event_DPP4_male["outcome_urinary_frequency_cA"]),
                     " ", as.numeric(n_event_DPP4_female["outcome_falls_cA"]),               as.numeric(n_event_DPP4_male["outcome_falls_cA"]),
                     " ", as.numeric(n_event_DPP4_female["outcome_llffalls_cA"]),            as.numeric(n_event_DPP4_male["outcome_llffalls_cA"]),
                     " ", as.numeric(n_event_DPP4_female["outcome_amputation_cA"]),          as.numeric(n_event_DPP4_male["outcome_amputation_cA"]),
                     " ", as.numeric(n_event_DPP4_female["outcome_dka_cA"]),                 as.numeric(n_event_DPP4_male["outcome_dka_cA"])
)                    


### additional information: incidence rates ----


personyears_atrisk_SGLT2_female   <- colSums(data[data$drugclass == "SGLT2" & data$gender_chr == 1, ][ ,followuptime_of_interest])/365.25
personyears_atrisk_SGLT2_male     <- colSums(data[data$drugclass == "SGLT2" & data$gender_chr == 0, ][ ,followuptime_of_interest])/365.25
personyears_atrisk_DPP4_female    <- colSums(data[data$drugclass == "DPP4"  & data$gender_chr == 1, ][ ,followuptime_of_interest])/365.25
personyears_atrisk_DPP4_male      <- colSums(data[data$drugclass == "DPP4"  & data$gender_chr == 0, ][ ,followuptime_of_interest])/365.25

incidence_rate_SGLT2_female     <- 1000*(n_event_SGLT2_female/personyears_atrisk_SGLT2_female)
incidence_rate_SGLT2_male       <- 1000*(n_event_SGLT2_male/    personyears_atrisk_SGLT2_male)
incidence_rate_DPP4_female      <- 1000*(n_event_DPP4_female/personyears_atrisk_DPP4_female)
incidence_rate_DPP4_male        <- 1000*(n_event_DPP4_male/    personyears_atrisk_DPP4_male)



incidence_rate_SGLT2 <-   c(" ", round(as.numeric(incidence_rate_SGLT2_female["outcome_gi_cA"]), 1),                      round(as.numeric(incidence_rate_SGLT2_male["outcome_gi_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_SGLT2_female["outcome_osmo_cA"]), 1),                    round(as.numeric(incidence_rate_SGLT2_male["outcome_osmo_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_SGLT2_female["outcome_micturition_control_cA"]), 1),     round(as.numeric(incidence_rate_SGLT2_male["outcome_micturition_control_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_SGLT2_female["outcome_volume_depletion_cA"]), 1),        round(as.numeric(incidence_rate_SGLT2_male["outcome_volume_depletion_cA"]), 1),  
                            " ", round(as.numeric(incidence_rate_SGLT2_female["outcome_urinary_frequency_cA"]), 1),       round(as.numeric(incidence_rate_SGLT2_male["outcome_urinary_frequency_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_SGLT2_female["outcome_falls_cA"]), 1),                   round(as.numeric(incidence_rate_SGLT2_male["outcome_falls_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_SGLT2_female["outcome_llffalls_cA"]), 1),                round(as.numeric(incidence_rate_SGLT2_male["outcome_llffalls_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_SGLT2_female["outcome_amputation_cA"]), 1),              round(as.numeric(incidence_rate_SGLT2_male["outcome_amputation_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_SGLT2_female["outcome_dka_cA"]), 1),                     round(as.numeric(incidence_rate_SGLT2_male["outcome_dka_cA"]), 1)
)  


incidence_rate_DPP4  <-   c(" ", round(as.numeric(incidence_rate_DPP4_female["outcome_gi_cA"]), 1),                       round(as.numeric(incidence_rate_DPP4_male["outcome_gi_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_DPP4_female["outcome_osmo_cA"]), 1),                     round(as.numeric(incidence_rate_DPP4_male["outcome_osmo_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_DPP4_female["outcome_micturition_control_cA"]), 1),      round(as.numeric(incidence_rate_DPP4_male["outcome_micturition_control_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_DPP4_female["outcome_volume_depletion_cA"]), 1),         round(as.numeric(incidence_rate_DPP4_male["outcome_volume_depletion_cA"]), 1),  
                            " ", round(as.numeric(incidence_rate_DPP4_female["outcome_urinary_frequency_cA"]), 1),        round(as.numeric(incidence_rate_DPP4_male["outcome_urinary_frequency_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_DPP4_female["outcome_falls_cA"]), 1),                    round(as.numeric(incidence_rate_DPP4_male["outcome_falls_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_DPP4_female["outcome_llffalls_cA"]), 1),                 round(as.numeric(incidence_rate_DPP4_male["outcome_llffalls_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_DPP4_female["outcome_amputation_cA"]), 1),               round(as.numeric(incidence_rate_DPP4_male["outcome_amputation_cA"]), 1),
                            " ", round(as.numeric(incidence_rate_DPP4_female["outcome_dka_cA"]), 1),                      round(as.numeric(incidence_rate_DPP4_male["outcome_dka_cA"]), 1)
)  



outcome_names           <- c("genital infection", "     female", "     male", "osmotic symptoms", "     female", "     male", 
                           "Micturition control", "     female", "     male", "Volume Depletion", "     female", "     male",
                           "Urinary frequency", "     female", "     male", "falls", "     female", "     male", 
                           "falls + lower limp fracture", "     female", "     male", "amputation", "     female", "     male", 
                           "DKA", "     female", "     male")

mean                    <- as.numeric(plot_data[, 1 , 3])
lower                   <- as.numeric(plot_data[, 2 , 3])
upper                   <- as.numeric(plot_data[, 3 , 3])

text_col                <- paste(round(mean, 3), rep(" (", length(mean)),round(lower, 3),", ",round(upper,3),rep(")", length(mean)), sep = "")
text_col[which(text_col == "NA (NA, NA)")] <- ""
data_plot_paper         <- data.frame(cbind(outcome_names, n_event_SGLT2, n_event_DPP4, incidence_rate_SGLT2, incidence_rate_DPP4,  mean, lower, upper, text_col))

data_plot_paper$mean  <- as.numeric(data_plot_paper$mean)
data_plot_paper$lower <- as.numeric(data_plot_paper$lower)
data_plot_paper$upper <- as.numeric(data_plot_paper$upper)



## make the plot ----

Cairo(file = "forestplot_IV_genderstratification_binary.png", 
      type = "png",
      units = "in", 
      width = 20,#10 
      height = 8, 
      pointsize = 12, 
      dpi = 72)

data_plot_paper|>
forestplot(labeltext = c(outcome_names, n_event_SGLT2, n_event_DPP4, incidence_rate_SGLT2, incidence_rate_DPP4, text_col),
           xlog = F,
           ci.vertices = TRUE,
           ci.vertices.height = 0.3, 
           boxsize = 0.4,
           zero = 1,
           col = fpColors(box = c("blue"), lines = c("black")),
           fn.ci_norm = c("fpDrawCircleCI"), 
           xlab = "relative risk (%)",
           txt_gp = fpTxtGp(cex = 1, xlab = gpar(cex = 1), ticks = gpar(cex = 1)),
           lty.ci = c(1),
           new_page = TRUE)|>
  fp_add_header(outcome_names = c("adverse effects",""), n_event_SGLT2 = c("n event", "(SGLT2)"), n_event_DPP4 = c("n event", "(DPP4)"), 
                incidence_rate_SGLT2 = c("incidence rate", "(SGLT2)"), incidence_rate_DPP4 = c("incidence rate", "(DPP4)"), text_col = c("RR (95% CI)","")) |>
  fp_add_lines(h_3 = gpar(lty = 1), h_6 = gpar(lty = 2), h_9 = gpar(lty = 2), h_12 = gpar(lty = 2),
               h_15 = gpar(lty = 2), h_18 = gpar(lty = 2), h_21 = gpar(lty = 2),
               h_24 = gpar(lty = 2), h_27 = gpar(lty = 2))|> 
  fp_decorate_graph(graph.pos = 6)


dev.off()





