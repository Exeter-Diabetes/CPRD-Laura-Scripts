#
# Instrumental variable method (by Ertefaie et al. 2017) -----------------------
#


## load needed data ----

load(paste0(data_path,"/study_cohort_",year_type,"_wIV.Rdata"))

if(population_type == "study_population"){
  
  data <- study_cohort_wIV
  
  }else if(population_type == "not_elderly"){
    
    data <- study_cohort_wIV[study_cohort_wIV$elderly == 0, ]
    
    }else if(population_type == "elderly"){
      
      data <- study_cohort_wIV[study_cohort_wIV$elderly == 1, ]
      
      }else if(population_type == "study_population_female"){
        
        data <- study_cohort_wIV[study_cohort_wIV$gender == 2, ]
        
        }else{
          
          data <- study_cohort_wIV[study_cohort_wIV$gender == 1, ]
          
          }

rm(study_cohort_wIV)


## data preparation ----

data$treatment    <- ifelse(data$drugclass == "DPP4", 0,1)

## define variable set ----

source(paste0("variableset_", which_outcome,".R"))

## Step1 ----

if(population_type == "study_population_female" | population_type == "study_population_male"){
  
  all_W_step1                 <- all_W_notgender[!all_W_notgender%in%c("prehba1c", "preegfr", "prebmi", "preweight")]
  
  }else{
    
    all_W_step1               <- all_W[!all_W%in%c("prehba1c", "preegfr", "prebmi", "preweight")]
    
    }

Ertefaie_AX_model_step1_formula <- as.formula(paste(X, " ~ ", paste0(c(all_W_step1, "(1|pracid)"), collapse =  " + ")))
Ertefaie_AX_model_step1         <- glmer(Ertefaie_AX_model_step1_formula, family = binomial("logit"), data = data)

b_j_hat             <- ranef(Ertefaie_model_step1)$pracid$"(Intercept)"
b_j_hat_median      <- median((exp(b_j_hat))/(1 + exp(b_j_hat)))
IV_j_ePP            <- ifelse((exp(b_j_hat))/(1 + exp(b_j_hat)) > b_j_hat_median, 1, 0)

data_IV             <- data.frame(cbind(as.numeric((rownames(ranef(Ertefaie_model_step1)$pracid))), IV_j_ePP))
colnames(data_IV)   <- c("pracid", "IV_ePP")

data_merged         <- merge(data, data_IV, by = "pracid")
data                <- data_merged

## Step 2 ----

Z                <- "IV_ePP"
variables_cc     <- c(Y, paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = ""), all_W, Z)
data_cc          <- data[complete.cases(data[ , variables_cc]), ]

followup_time    <- data_cc[ , paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = "")]


if(population_type == "study_population_female" | population_type == "study_population_male"){
  
  IV_Ertefaie_model1_formula  <- as.formula(paste(X, " ~ ", paste0(c(all_W_notgender, Z), collapse =  " + ")))
  
  }else{
    
    IV_Ertefaie_model1_formula  <- as.formula(paste(X, " ~ ", paste0(c(all_W, Z), collapse =  " + ")))
    
    }

IV_Ertefaie_model1             <- glm(IV_Ertefaie_model1_formula, family = binomial, data = data_cc)

data_cc$X_hat_IV_Ertefaie      <- as.numeric(IV_Ertefaie_model1$fitted.values)
X_hat                          <- "X_hat_IV_Ertefaie"


if(population_type == "study_population_female" | population_type == "study_population_male"){
  
  IV_Ertefaie_model2_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X_hat, all_W_notgender), collapse =  " + ")))
  
  }else{
    
    IV_Ertefaie_model2_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X_hat, all_W), collapse =  " + ")))
    
    }


if(outcome_variable_type == "binary"){
  
  IV_Ertefaie_model2             <- glm(IV_Ertefaie_model2_formula, offset = log(followup_time), family = poisson(link = log), data = data_cc)
  
}else{
  
  IV_Ertefaie_model2             <- lm(IV_Ertefaie_model2_formula, data = data_cc)
  
}


## Extraction of the results ----

assign(paste0("IV_Ertefaie_",which_outcome,"_model_summary"), summ(IV_Ertefaie_model2, confint = TRUE, digits = 4))
assign(paste0("IV_Ertefaie_",which_outcome,"_model1"), IV_Ertefaie_model1)

