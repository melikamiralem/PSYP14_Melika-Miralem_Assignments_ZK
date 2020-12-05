#######Assignment 1: Hierarchical regression

###Installing and loading R-packages for the analysis
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(psych)
library(lm.beta)
library(car)
library(lmtest)
library(sandwich)
library(boot)
library(lmboot)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(optimx)

###Custom function for getting the coefficient table
coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}

###Custom function for standardized coefficients
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <-apply(getME(object,"X"), 2, sd)
  sc <-fixef(object)*sdx/sdy
  se.fixef <-coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
  }

###Loading the data-sets on which the statistical analysis will be performed
data_file_3 = read.csv("https://tinyurl.com/ha-dataset3")
data_file_4 = read.csv("https://tinyurl.com/ha-dataset4")

###Data check and descriptive statistics
View(data_file_3)
describe(data_file_3)
summary(data_file_3)
str(data_file_3)

View(data_file_4)
describe(data_file_4)
summary(data_file_4)
str(data_file_4)

###Outlier exclusion
#Participant ID_21 will be excluded from the first data-set, due to impossible income value (-6994). Also, participant with ID ID_182 has a spelling mistake in data entry for sex (femlae), so this will be replaced with correct spelling.
data_file_3_new <- data_file_3
data_file_3_new <- data_file_3_new[-c(21), ]
data_file_3_new <- data_file_3 %>% 
  mutate(sex = replace(sex, sex == "femlae", "female"))
View(data_file_3_new)
describe(data_file_3_new)
summary(data_file_3_new)
#Participant ID_80 will be excluded from the second data-set, due to having result above the maximum of the measurement scale for mindfulness (his/her result is 6.05, maximum is 6). Also, participants ID_5 and ID_87 will be excluded for having impossible values for household_income, i.e. -23482 and -3409, respectively.
data_file_4_new <- data_file_4
data_file_4_new <- data_file_4_new[-c(80, 5, 87), ]
View(data_file_4_new)
describe(data_file_4_new)
summary(data_file_4_new)

###Building a linear mixed, random intercept model

data_file_3_new = data_file_3_new %>%
  mutate(hospital = recode_factor(hospital,
                                  "hospital_1" = 1,
                                  "hospital_2" = 2,
                                  "hospital_3" = 3,
                                  "hospital_4" = 4,
                                  "hospital_5" = 5,
                                  "hospital_6" = 6,
                                  "hospital_7" = 7,
                                  "hospital_8" = 8,
                                  "hospital_9" = 9,
                                  "hospital_10" = 10))

data_file_4_new = data_file_4_new %>%
  mutate(hospital = recode_factor(hospital,
                                  "hospital_11" = 11,
                                  "hospital_12" = 12,
                                  "hospital_13" = 13,
                                  "hospital_14" = 14,
                                  "hospital_15" = 15,
                                  "hospital_16" = 16,
                                  "hospital_17" = 17,
                                  "hospital_18" = 18,
                                  "hospital_19" = 19,
                                  "hospital_20" = 20))

rndm_int_model = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data = data_file_3_new)
rndm_int_model

###Building a linear regression model based on Assignment 1
assignment_1_model = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_file_3_new)
assignment_1_model

###Getting the coefficients for model from Assignment 1
coef_table(assignment_1_model)

###Marginal R squared values of the fixed factors
r2beta(rndm_int_model, method = "nsj", data = data_file_3_new)

###Effects coefficients
summary(rndm_int_model)

###Estimation of marginal and conditional R squared value
r.squaredGLMM(rndm_int_model)

###Confidence intervals
confint (rndm_int_model)

###Standardized beta for predictors
stdCoef.merMod(rndm_int_model)

###Calculating cAIC for random intercept model
cAIC(rndm_int_model)

###Pain prediction based on random intercept model
rndm_int_model_pred <- predict(rndm_int_model, data_file_4_new, allow.new.levels=TRUE)
rndm_int_model_pred

###Calculating random intercept model mean
rndm_int_model_mean = lmer(pain ~ 1 + (1|hospital), data = data_file_4_new)
rndm_int_model_mean

###Calculating RSS and TSS to obtain variance explained of pain from data_file_4_new
model_mean <- lm(pain ~ 1, data = data_file_4_new)
RSS_rndm_int_model = sum(residuals(rndm_int_model)^2)
TSS_rndm_int_model = sum((data_file_4_new$pain - predict(model_mean))^2)

###Creating variance
1-RSS_rndm_int_model/TSS_rndm_int_model
r2beta(rndm_int_model)
#variance = 0.3954969

###Creating random slope model
summary(rndm_int_model)
#we choose cortisol_serum because it is the most significant (influential) one
rndm_slope_model = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_file_3_new)
rndm_slope_model

###Visualization of the fitted regression lines for each hospital separately
data_file_3_new = data_file_3_new %>%
  mutate(pred_slope_model = predict(rndm_slope_model))
data_file_3_new %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y = pred_slope_model, x = cortisol_serum)) +
  facet_wrap(~ hospital, ncol = 2)
