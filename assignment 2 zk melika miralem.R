#######Assignment 2

###Installing and loading R-packages for the analysis
install.packages("sandwich")
install.packages("boot")
install.packages("lmboot")
install.packages("cAIC4")
install.packages("r2glmm")
install.packages("lme4")
install.packages("lmerTest")
install.packages("MuMIn")
install.packages("optimx")

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

###Loading the data-set on which the statistical analysis will be performed
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

###Data check and descriptive statistics
View(data_sample_1)
describe(data_sample_1)
summary(data_sample_1)
str(data_sample_1)

###Outlier exclusion
#Participants with IDs ID_93, ID_150, and ID_109 will be excluded from the data-set, due to impossible life age number (444), incomplete result on STAI-T questionnaire (3,9; minimum is 20), and the household income expressed with a minus sign (-3732), respectively.
data_sample_1_new <- data_sample_1
data_sample_1_new <- data_sample_1_new[-c(93, 150, 109), ]
View(data_sample_1_new)
describe(data_sample_1_new)
summary(data_sample_1_new)

#####Creating initial model (used by fellow researcher)
initial_model <-  lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1_new)
summary(initial_model)

###Outliers check for initial model
#Residual-leverage plot
initial_model %>%
  plot(which = 5)

#Cook's distance
initial_model %>%
  plot(which = 4)

#####Linear regression assumptions check for initial model
###Normality check
#QQ plot
initial_model %>%
  plot(which = 2)
#Histogram
residuals_initial_model = enframe(residuals(initial_model))
residuals_initial_model %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()
#Describe function
describe(residuals(initial_model))

###Linearity check
initial_model %>%
  residualPlots()

###Homoscedasticity check
initial_model %>%
  plot(which = 3)
#Breusch-Pagan test
initial_model %>%
  bptest()
#NCV test
initial_model %>%
  ncvTest()

###Multicollinearity check
initial_model %>%
  vif()

initial_model

#####Creating backward regression model
backward_model = step(initial_model, direction = "backward")
backward_model <- backward_model
backward_model
summary(backward_model)

###Outliers check for backward model
#Residual-leverage plot
backward_model %>%
  plot(which = 5)
#Cook's distance
backward_model %>%
  plot(which = 4)

#####Linear regression assumptions check for backward model

###Normality check
#QQ plot
backward_model %>%
  plot(which = 2)
#Histogram
residuals_backward_model = enframe(residuals(backward_model))
residuals_backward_model %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()
#Describe function
describe(residuals(backward_model))

###Linearity check
backward_model %>%
  residualPlots()

###Homoscedasticity check
backward_model %>%
  plot(which = 3)
#Breusch-Pagan test
backward_model %>%
  bptest()
#NCV test
backward_model %>%
  ncvTest()

###Multicollinearity check
backward_model %>%
  vif()

###Backward model summary
summary(backward_model)
confint(backward_model)
lm.beta(backward_model)

#####Assignment 1 final regression model, i.e. theory based model
theory_based_model <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_new)

#####Comparison: theory-based model, initial model (fellow researcher's model) and backward model
anova(theory_based_model, initial_model)
anova(theory_based_model, backward_model)
anova(initial_model, backward_model)
AIC (theory_based_model)
AIC (initial_model)
AIC (backward_model)
summary (theory_based_model)
summary (initial_model)
summary (backward_model)

#####Checking efficiency of the models on a new data-set
data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

###Data check
View(data_sample_2)
describe(data_sample_2)
summary(data_sample_2)

#####Pain prediction
theory_based_model_prediction <- predict(theory_based_model, data_sample_2)
backward_model_prediction <- predict(backward_model, data_sample_2)
RSS_theory_based = sum((data_sample_2$pain - theory_based_model_prediction)^2)
RSS_backward = sum((data_sample_2$pain - backward_model_prediction)^2)
RSS_theory_based
RSS_backward


