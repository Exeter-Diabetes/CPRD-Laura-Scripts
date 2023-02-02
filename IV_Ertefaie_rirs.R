#
# AX - Instrumental variable method (by Ertefaie et al. 2017) - rirs -----------
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
  
  all_W_step1               <- all_W_notgender[!all_W_notgender%in%c("prehba1c", "preegfr", "prebmi", "preweight")]
  
}else{
  
  all_W_step1               <- all_W[!all_W%in%c("prehba1c", "preegfr", "prebmi", "preweight")]
  
}

T                           <-  "tx_order"

Ertefaie_rirs_model_step1_formula <- as.formula(paste(X, " ~ ", paste0(c(all_W_step1, T, "(tx_order|pracid)"), collapse =  " + ")))
Ertefaie_rirs_model_step1         <- glmer(Ertefaie_rirs_model_step1_formula, family = binomial("logit"), data = data)

b0_j_hat            <- ranef(Ertefaie_rirs_model_step1)$pracid$"(Intercept)"
b1_j_hat            <- ranef(Ertefaie_rirs_model_step1)$pracid$"tx_order"

data_IV             <- data.frame(cbind(as.numeric((rownames(ranef(Ertefaie_rirs_model_step1)$pracid))), b0_j_hat, b1_j_hat))
colnames(data_IV)   <- c("pracid", "b0_j_hat", "b1_j_hat")

data_IV$B0_hat      <- rep(summary(Ertefaie_rirs_model_step1)$coef["(Intercept)", "Estimate"],  times = dim(data_IV)[1])
data_IV$BT_hat      <- rep(summary(Ertefaie_rirs_model_step1)$coef["tx_order", "Estimate"],     times = dim(data_IV)[1])

data_merged         <- merge(data, data_IV, by = "pracid")

fitted_hat          <- data_merged$B0_hat + data_merged$b0_j_hat + (data_merged$BT_hat + data_merged$b1_j_hat)*data_merged$tx_order

fitted_hat_median   <- median((exp(fitted_hat))/(1 + exp(fitted_hat)))
data$IV_ePP_rirs    <- ifelse((exp(fitted_hat))/(1 + exp(fitted_hat)) > fitted_hat_median, 1, 0)


## Step 2 ----
 
Z                <- "IV_ePP_rirs"
variables_cc     <- c(Y, paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = ""), all_W, Z)
data_cc          <- data[complete.cases(data[ , variables_cc]), ]

followup_time    <- data_cc[ , paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = "")]


if(population_type == "study_population_female" | population_type == "study_population_male"){
  
  IV_Ertefaie_rirs_model2_formula     <- as.formula(paste(X, " ~ ", paste0(c(all_W_notgender, Z), collapse =  " + ")))
  
  }else{
    
    IV_Ertefaie_rirs_model1_formula   <- as.formula(paste(X, " ~ ", paste0(c(all_W, Z), collapse =  " + ")))
    
    }


IV_Ertefaie_rirs_model1             <- glm(IV_Ertefaie_rirs_model1_formula, family = binomial, data = data_cc)

data_cc$X_hat_IV_Ertefaie_rirs      <- as.numeric(IV_Ertefaie_rirs_model1$fitted.values)
X_hat                               <- "X_hat_IV_Ertefaie_rirs"


if(population_type == "study_population_female" | population_type == "study_population_male"){
  
  IV_Ertefaie_rirs_model2_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X_hat, all_W_notgender), collapse =  " + ")))
  
  }else{
    
    IV_Ertefaie_rirs_model2_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X_hat, all_W), collapse =  " + ")))
    
    }



if(outcome_variable_type == "binary"){
  
  IV_Ertefaie_rirs_model2             <- glm(IV_Ertefaie_rirs_model2_formula, offset = log(followup_time), family=poisson(link = log), data = data_cc)
  
}else{
  
  IV_Ertefaie_rirs_model2             <- lm(IV_Ertefaie_rirs_model2, data = data_cc)
  
}


## Extraction of the results ----

assign(paste0("IV_Ertefaie_rirs_",which_outcome,"_model_summary"), summ(IV_Ertefaie_rirs_model2, confint = TRUE, digits = 4))
assign(paste0("IV_Ertefaie_rirs_",which_outcome,"_model1"), IV_Ertefaie_rirs_model1)





