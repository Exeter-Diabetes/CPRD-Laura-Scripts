#
# Summary and result plots -----------------------------------------------------
#

# https://mcfromnz.wordpress.com/2012/11/06/forest-plots-in-r-ggplot-with-side-table/

#
# Packages ---------------------------------------------------------------------
#

# install.packages("ggplot2")
library(ggplot2)

# install.packages("Cairo")
library(Cairo)

#
# Set paths --------------------------------------------------------------------
#

setwd("C:/Users/lg531/OneDrive - University of Exeter/Studies/Application_Study/analysis_CPRD_Aurum/triangulation_analysis")

result_path <- "C:/Users/lg531/OneDrive - University of Exeter/Studies/Application_Study/analysis_CPRD_Aurum/triangulation_analysis/results"


#
# Decision on analysis ---------------------------------------------------------
#

outcome_interest <- c("GI", "OS", "AX", "HbA1c", "weightchange", "weight")[6]   # change for which outcome plots are computed
year_type        <- c("1year","3year")[2]                                       # change year according to outcome follow up (sensitivity analysis)
censoring_type   <- c("A","B")[1]                                               # change according to censoring type (sensitivity analysis)

results_populations <- c("study_population", "not_elderly", "elderly")
results_models      <- c("MVR", "PSM", "IV_prevpatient", "IV_Ertefaie", "IV_Ertefaie_rirs")
treatment_name      <- c("treatment","treatment", "X_hat_IV_prevpatient", "X_hat_IV_Ertefaie",  "X_hat_IV_Ertefaie_rirs")
results_outcomes    <- c("GI", "OS", "AX", "HbA1c", "weightchange", "weight")

reference_line <- ifelse(outcome_interest == "HbA1c" | outcome_interest == "weightchange" | outcome_interest == "weight" , 0, 1)
x_scale_name   <- ifelse(outcome_interest == "HbA1c" | outcome_interest == "weightchange" | outcome_interest == "weight", "Estmate", "relative risk (%)")

#
# Build empty results table ----------------------------------------------------
#


rownumber   <- length(results_models)*length(results_populations)
colnumber   <- 5

result_data           <- as.data.frame(matrix(NA, nrow = rownumber, ncol = colnumber))
colnames(result_data) <- c("cohort", "causal_estimate", "lower", "upper", "methods")
result_data$cohort    <- c(rep("study_population", times = length(results_models)), rep("not_elderly", times = length(results_models)) , rep("elderly", times = length(results_models)))
result_data$methods   <- rep(results_models, times = length(results_populations))


#
# Load the result data in results table ----------------------------------------
#


for(p in 1:length(results_populations)){
  
  for(m in 1:length(results_models)){
    
    subset_results <- get(load(paste0(result_path, "/",results_models[m], "_", outcome_interest, "_modelsummary_", year_type, "_c", censoring_type, "_",results_populations[p],".Rdata" )))
    
    if(outcome_interest == "HbA1c" | outcome_interest == "weightchange" | outcome_interest == "weight"){
       
      result_data$causal_estimate[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]] <- (subset_results$coeftable[treatment_name[m], "Est."])
      result_data$lower[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]]           <- (subset_results$coeftable[treatment_name[m], "2.5%"])
      result_data$upper[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]]           <- (subset_results$coeftable[treatment_name[m], "97.5%"])
      
      
    }else{
      
      result_data$causal_estimate[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]] <- exp(subset_results$coeftable[treatment_name[m], "Est."])
      result_data$lower[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]]           <- exp(subset_results$coeftable[treatment_name[m], "2.5%"])
      result_data$upper[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]]           <- exp(subset_results$coeftable[treatment_name[m], "97.5%"])
      
      
    }
    
    
    rm(subset_results)
  }
  
}

result_data$methods <- factor(result_data$methods, level = rev(results_models))
result_data$cohort  <- factor(result_data$cohort, level =  rev(results_populations))


#
# Create result plot  ----------------------------------------------------------
#


dotCOLS = c("#88bf93","#91bdc4", "#986d9e", "#b06f7f", "#e39c78")        # lighter colours
barCOLS = c("#48A65B","#48A6B5", "#632d6b", "#B53656", "#e8763c")        # darker colours

p <- ggplot(result_data, aes(x=cohort, y=causal_estimate, ymin=lower, ymax=upper,col=methods,fill=methods)) + 
  #specify position here
  geom_linerange(size=3,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=reference_line, lty=2) +
  #specify position here too
  geom_point(size=3, shape=21, colour= "white", stroke = 0.5, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS)+
  scale_color_manual(values=dotCOLS)+
  scale_x_discrete(name="") +
  scale_y_continuous(name=x_scale_name) + # , limits = c(1,4)
  coord_flip() +
  theme_minimal()+
  theme(
    panel.background = element_rect(fill="white"), #transparent panel bg
    plot.background = element_rect(fill="white", color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill="white"), #transparent legend bg
    legend.box.background = element_rect(fill="white", color = NA) #transparent legend panel
  ) +
  theme(axis.text=element_text(size=12)) + 
  theme(legend.text=element_text(size=12)) +
  theme(legend.title=element_blank())+
  theme(legend.key.size = unit(1, 'cm'))+
  ggtitle(paste0(outcome_interest, " - model results"," (",year_type," follow up, censoring type ",  censoring_type, ")"))
  


Cairo(file = paste0(result_path,"/estimation_results_",outcome_interest,"_",year_type,"_c",censoring_type,".png"), 
      type = "png",
      units = "in", 
      width = 10, 
      height = 8, 
      pointsize = 12, 
      dpi = 72)


plot(p)


dev.off()








