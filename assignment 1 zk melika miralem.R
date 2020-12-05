#######Assignment 1: Hierarchical regression########

###Installing and loading R-packages for the analysis
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("psych")
install.packages("lm.beta")
install.packages("car")
install.packages("lmtest")

library(tidyverse)
library(gridExtra)
library(psych)
library(lm.beta)
library(car)
library(lmtest)

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

#####Creating Model 1
model_1 <- lm(pain ~ sex + age, data = data_sample_1_new)
summary(model_1)

###Visualization - Model 1
scatterplot1 = data_sample_1_new %>% 
  ggplot() + 
  aes(x = age, y = pain) +
  geom_point(shape = 21, fill = "seagreen2", size = 3) + 
  geom_smooth(method = "lm")
scatterplot1

scatterplot2 = data_sample_1_new %>% 
  ggplot() + 
  aes(x = sex, y = pain) + 
  geom_point(shape = 22, fill = "violetred", size = 3)
scatterplot2

grid.arrange(scatterplot1, scatterplot2, nrow = 1)

###Outliers check for Model 1
#Regression scatterplot
grid.arrange(scatterplot1, scatterplot2, nrow = 1)
#Residual-leverage plot
model_1 %>%
  plot(which = 5)
#Cook's distance
model_1 %>%
  plot(which = 4)

#####Linear regression assumptions check for Model 1

###Normality check
#QQ plot
model_1 %>%
  plot(which = 2)
#Histogram
residuals_model_1 = enframe(residuals(model_1))
residuals_model_1 %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()
#Describe function
describe(residuals(model_1))

###Linearity check
model_1 %>%
  residualPlots()

###Homoscedasticity check
model_1 %>%
  plot(which = 3)
#Breusch-Pagan test
model_1 %>%
  bptest()
#NCV test
model_1 %>%
  ncvTest()

###Multicollinearity check
model_1 %>%
  vif()

###Model 1 report
model_1
summary (model_1)
confint(model_1)
lm.beta(model_1)

#####Creating Model 2
model_2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1_new)
summary(model_2)

###Visualization - remaining variables of Model 2
scatterplot3 = data_sample_1_new %>% 
  ggplot() + 
  aes(x = STAI_trait, y = pain) +
  geom_point(shape = 21, fill = "tan2", size = 3) + 
  geom_smooth(method = "lm")
scatterplot3

scatterplot4 = data_sample_1_new %>% 
  ggplot() + 
  aes(x = pain_cat, y = pain) + 
  geom_point(shape = 21, fill = "tan2", size = 3) +
  geom_smooth(method = "lm")
scatterplot4

scatterplot5 = data_sample_1_new %>% 
  ggplot() + 
  aes(x = mindfulness, y = pain) + 
  geom_point(shape = 21, fill = "tan2", size = 3) +
  geom_smooth(method = "lm")
scatterplot5

scatterplot6 = data_sample_1_new %>% 
  ggplot() + 
  aes(x = cortisol_serum, y = pain) + 
  geom_point(shape = 21, fill = "tan2", size = 3) +
  geom_smooth(method = "lm")
scatterplot6

scatterplot7 = data_sample_1_new %>% 
  ggplot() + 
  aes(x = cortisol_saliva, y = pain) + 
  geom_point(shape = 21, fill = "tan2", size = 3) +
  geom_smooth(method = "lm")
scatterplot7

grid.arrange(scatterplot3, scatterplot4, scatterplot5, scatterplot6, scatterplot7, nrow = 3)

###Outliers check for Model 2
#Residual-leverage plot
model_2 %>%
  plot(which = 5)

#Cook's distance
model_2 %>%
  plot(which = 4)

#####Linear regression assumptions check for Model 2
###Normality check
#QQ plot
model_2 %>%
  plot(which = 2)
#Histogram
residuals_model_2 = enframe(residuals(model_2))
residuals_model_2 %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()
#Describe function
describe(residuals(model_2))

###Linearity check
model_2 %>%
  residualPlots()

###Homoscedasticity check
model_2 %>%
  plot(which = 3)
#Breusch-Pagan test
model_2 %>%
  bptest()
#NCV test
model_2 %>%
  ncvTest()

###Multicollinearity check
model_2 %>%
  vif()

#####Creating new Model 2 (variable cortisol_saliva excluded)
model_2_new <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_new)
summary(model_2_new)

###Outliers check for new Model 2
#Residual-leverage plot
model_2_new %>%
  plot(which = 5)

#Cook's distance
model_2_new %>%
  plot(which = 4)

#####Linear regression assumptions check for new Model 2
###Normality check
#QQ plot
model_2_new %>%
  plot(which = 2)
#Histogram
residuals_model_2_new = enframe(residuals(model_2_new))
residuals_model_2_new %>%
  ggplot() +
  aes(x = value) +
  geom_histogram()
#Describe function
describe(residuals(model_2_new))

###Linearity check
model_2_new %>%
  residualPlots()

###Homoscedasticity check
model_2_new %>%
  plot(which = 3)
#Breusch-Pagan test
model_2_new %>%
  bptest()
#NCV test
model_2_new %>%
  ncvTest()

###Multicollinearity check
model_2_new %>%
  vif()

###New Model 2 report
model_2_new
summary (model_2_new)
confint(model_2_new)
lm.beta(model_2_new)

#####Model 1 and (new) Model 2 comparison
#r-square
summary(model_1)$adj.r.squared
summary(model_2_new)$adj.r.squared
#anova
anova(model_1, model_2_new)
#AIC
AIC(model_1)
AIC(model_2_new)
